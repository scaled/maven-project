//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// http://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path}
import pomutil.POM
import scaled._
import scaled.pacman.Filez
import scaled.util.{BufferBuilder, Errors}

class MavenProject (ps :ProjectSpace, r :Project.Root) extends AbstractFileProject(ps, r) {
  import Project._

  private def isMain = root.module.length == 0
  private def projName (isMain :Boolean) = pom.artifactId + (if (isMain) "" else "-test")

  private val pomFile = root.path.resolve("pom.xml")
  private var _pom :POM = null
  private def pom = if (_pom == null) throw Errors.feedback(s"Project not ready: $root") else _pom

  private val java = new JavaComponent(this)
  addComponent(classOf[JavaComponent], java)

  private def loadPOM :POM = POM.fromFile(pomFile.toFile) getOrElse {
    throw new IllegalArgumentException(s"Unable to load $pomFile")
  }
  // used to get our POM during transitive dependency resolution; this will be called on a
  // background thread, so doing the extra processing of loading the POM immediately is OK
  def getOrLoadPOM :POM = if (_pom == null) loadPOM else _pom

  // watch the POM file and any local parents for changes
  private def watchPOM (pom :POM) :Unit = {
    def watch (file :Path) = metaSvc.service[WatchService].watchFile(file, _ => reinit())
    pom.file foreach { f => watch(f.toPath) }
    pom.parent foreach { watchPOM }
  }
  // note that we don't 'close' our watches, we'll keep them active for the lifetime of the editor
  // because it's low overhead; I may change my mind on this front later, hence this note

  override protected def computeMeta (oldMeta :Project.Meta) =
    pspace.wspace.exec.runAsync { loadPOM } map { finishInit(oldMeta, _) }

  private def finishInit (oldMeta :Project.Meta, pom :POM) = {
    // set up our watch on the first init
    if (_pom == null) watchPOM(pom)
    _pom = pom

    val isMain = this.isMain
    if (isMain) {
      val troot = Root(root.path, "test")
      testSeedV() = Some(Seed(troot, projName(false), true, getClass, List(troot)))
    }

    // init our Java component
    val targetPre = pom.buildProps.getOrElse("directory", "target")
    val mainOutputDir = buildDir("outputDirectory", s"$targetPre/classes")
    val classesDir = if (isMain) mainOutputDir
    else buildDir("testOutputDirectory", s"$targetPre/test-classes")
    java.javaMetaV() = java.javaMetaV().copy(
      classes = Seq(classesDir),
      outputDir = classesDir,
      buildClasspath = classesDir +: (if (isMain) _depends.buildClasspath
                                      else mainOutputDir +: _depends.testClasspath),
      execClasspath = classesDir +: _depends.execClasspath
    )
    java.addTesters()

    // add dirs to our ignores
    val igns = FileProject.stockIgnores
    igns += FileProject.ignorePath(targetDir, root.path)
    if (!outputDir.startsWith(targetDir)) igns += FileProject.ignorePath(outputDir, root.path)
    ignores() = igns

    // TODO: this is expensive, can we do something cheaper
    val ssum = summarizeSources
    // TODO: do we want to try to support multi-lingual projects? that sounds like a giant PITA,
    // but we could probably at least generate a warning if we have some crazy mishmash of sources

    // TEMP: if we have any Kotlin files, we just use the KotlinCompiler
    if (ssum.contains("kt")) {
      addComponent(classOf[Compiler], new KotlinCompiler(this, java) {
        // this will be the same for main and test projects, putting them in the same module;
        // TODO: except it doesn't work and seems to cause other problems; sigh override
        // def moduleName = Some(pom.artifactId)
        override def javacOpts = MavenProject.this.javacOpts
        override def kotlincOpts = MavenProject.this.kotlincOpts
        override def kotlincVers = MavenProject.this.kotlincVers
        override protected def willCompile () = copyResources()
      })

    } else {
      addComponent(classOf[Compiler], new ScalaCompiler(this, java) {
        override def javacOpts = MavenProject.this.javacOpts
        override def scalacOpts = MavenProject.this.scalacOpts
        override def scalacVers = MavenProject.this.scalacVers
        override protected def willCompile () = copyResources()
      })
    }

    oldMeta.copy(
      name = projName(isMain),
      ids = {
        val ids = Seq.builder[Id]()
        // TODO: we could have our RepoId support a classifier and have our id be classified as
        // tests for the test sub-project, but for now we just use artifactId-test
        ids += RepoId(MavenRepo, pom.groupId, name, pom.version)
        // if we have VCS URLs in our POM, make ID from those too
        if (isMain) ids ++= {
          def stripSCM (url :String) = if (url startsWith "scm:") url.substring(4) else url
          Option.from(pom.scm.connection) map(stripSCM(_).split(":", 2)) collect {
            case Array(vcs, url) => SrcURL(vcs, url)
          }
        }
        ids.build()
      },
      sourceDirs = (if (isMain) allLangs(buildDir("sourceDirectory", "src/main/java"))
                    else allLangs(buildDir("testSourceDirectory", "src/test/java")))
    )
  }

  override def addToBuffer (buffer :RBuffer) {
    super.addToBuffer(buffer)
  }

  override def testSeed = testSeedV()
  private val testSeedV = Value[Option[Seed]](None)

  override def depends = {
    val deps = Seq.builder[Id]
    if (isMain) deps ++= _depends.buildTransitive
    else {
      // if this is the test subproject, add a depend on the main project
      deps += RepoId(MavenRepo, pom.groupId, pom.artifactId, pom.version)
      deps ++= _depends.testTransitive
    }
    deps += _depends.platformDepend
    deps.build()
  }

  private def allLangs (java :Path) :Seq[Path] = {
    // if the java path is not of the form foo/java then we can't langify it
    if (java.getFileName.toString != "java") Seq(java)
    // otherwise turn foo/java into foo/scala, etc.
    else (Seq(java) ++ Seq("scala", "kotlin", "kt").map(java.getParent.resolve(_))).
      filter(Files.exists(_))
  }

  private def targetDir = buildDir("directory", "target")

  private def outputDir = component(classOf[JavaComponent]).get.outputDir
  private def copyResources () {
    (if (isMain) pom.resources else pom.testResources) foreach copyResources(outputDir)
  }

  private def javacOpts :Seq[String] = {
    // this returns 0-N Plugin instances (one for each POM in the parent chain)
    val cps  = pom.plugin("org.apache.maven.plugins", "maven-compiler-plugin")
    // look for source/target configuration
    cps.flatMap(_.configValue("source")).takeRight(1).fromScala.flatMap(List("-source", _)) ++
      cps.flatMap(_.configValue("target")).takeRight(1).fromScala.flatMap(List("-target", _)) ++
      // also look for <compilerArgs> sections
      cps.flatMap(_.configList("compilerArgs", "arg")).fromScala
  }

  private def scalacOpts :Seq[String] = {
    // look for info from either maven-scala or scala-maven plugins
    val msp = pom.plugin("org.scala-tools", "maven-scala-plugin")
    val smp = pom.plugin("net.alchim31.maven", "scala-maven-plugin")
    // the above returns info for every POM up the chain of parents, so we flatten any config
    // directives we find therein into one
    (if (msp.isEmpty) smp else msp).flatMap(_.configList("args", "arg")).fromScala
  }

  private def kotlincOpts :Seq[String] = {
    // this above returns info for every POM up the chain of parents, so we flatten any config
    // directives we find therein into one
    pom.plugin("org.jetbrains.kotlin", "kotlin-maven-plugin").
      flatMap(_.configList("args", "arg")).fromScala
  }

  private def scalacVers :String = (depends collectFirst {
    case RepoId(_, "org.scala-lang", "scala-library", version) => version
  }) getOrElse ScalaCompiler.DefaultScalacVersion

  private def kotlincVers :String = (depends collectFirst {
    case RepoId(_, "org.jetbrains.kotlin", "kotlin-stdlib", version) => version
  }) getOrElse KotlinCompiler.DefaultKotlincVersion

  private def copyResources (target :Path)(rsrc :POM.Resource) {
    if (rsrc.targetPath.isDefined || rsrc.filtering || !rsrc.includes.isEmpty ||
        !rsrc.excludes.isEmpty) metaSvc.log.log("Complex <resources> not yet supported " + rsrc)
    else {
      val rsrcDir = root.path.resolve(rsrc.directory)
      if (Files.exists(rsrcDir)) Filez.copyAll(rsrcDir, target)
    }
  }

  private val _depends = new Depends(pspace) {
    def pom = MavenProject.this.pom
  }
  private def buildDir (key :String, defpath :String) :Path =
    root.path.resolve(pom.buildProps.getOrElse(key, defpath))
}

object MavenProject {

  @Plugin(tag="project-finder")
  class FinderPlugin extends ProjectFinderPlugin("maven", true, classOf[MavenProject]) {
    def checkRoot (root :Path) :Int = if (exists(root, "pom.xml")) 1 else -1
    override protected def mkRoot (seed :Path, path :Path) = {
      val ppath = path.relativize(seed)
      // TODO: really we need to read the POM, look for testSourceDir and check whether we're
      // inside there, but jesus fuck I can't be bothered at the moment
      if (ppath.exists(_.getFileName.toString == "test")) Project.Root(path, "test")
      else Project.Root(path)
    }
  }
}
