
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// http://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path}
import pomutil.POM
import scaled._
import scaled.pacman.Filez

object MavenPlugins {
  import Project._

  val PomFile = "pom.xml"

  @Plugin(tag="project-root")
  class MavenRootPlugin extends RootPlugin.File(PomFile) {

    override protected def createRoot (paths :List[Path], path :Path) = {
      val seed = paths.head
      val ppath = path.relativize(seed)
      // TODO: really we need to read the POM, look for testSourceDir and check whether we're
      // inside there, but jesus fuck I can't be bothered at the moment
      if (ppath.exists(_.getFileName.toString == "test")) Root(path, "test")
      else Root(path)
    }
  }

  @Plugin(tag="project-resolver")
  class MavenResolverPlugin extends ResolverPlugin {
    override def metaFiles (root :Project.Root) = Seq(root.path.resolve(PomFile))
    override def addComponents (project :Project) {
      val pomFile = project.root.path.resolve(PomFile)
      project.pspace.wspace.exec.runAsync(Maven.loadPOM(pomFile)).
        onSuccess(addMavenComponents(project, _)).
        onFailure(project.pspace.wspace.exec.handleError)
    }
  }

  // TODO: we could perhaps provide a way for resolver plugins to add additional to be watched
  // files; I'd rather not have to parse the whole POM and discover local parents in metaFiles and
  // then do that all again, maybe we should just have a cache for POM files...

  // // watch the POM file and any local parents for changes
  // private def watchPOM (pom :POM) :Unit = {
  //   val watchSvc = metaSvc.service[WatchService]
  //   def watch (file :Path) = toClose += watchSvc.watchFile(file, _ => reinit())
  //   pom.file foreach { f => watch(f.toPath) }
  //   pom.parent foreach { watchPOM }
  // }

  private def addMavenComponents (project :Project, pom :POM) {
    val rootPath = project.root.path
    val isMain = project.root.module.length == 0
    def projName (isMain :Boolean) = pom.artifactId + (if (isMain) "" else "-test")

    def buildDir (key :String, defpath :String) :Path =
      rootPath.resolve(pom.buildProps.getOrElse(key, defpath))
    val targetDir = buildDir("directory", "target")

    val targetPre = pom.buildProps.getOrElse("directory", "target")
    val mainOutputDir = buildDir("outputDirectory", s"$targetPre/classes")
    val classesDir = if (isMain) mainOutputDir
                     else buildDir("testOutputDirectory", s"$targetPre/test-classes")

    // add a filer component with custom ignores
    val igns = Ignorer.stockIgnores
    igns += Ignorer.ignorePath(targetDir, rootPath)
    if (!classesDir.startsWith(targetDir)) igns += Ignorer.ignorePath(classesDir, rootPath)
    project.addComponent(classOf[Filer], new DirectoryFiler(project, igns))

    // add our dependency management component
    val depends = new MavenDepends(project, pom, isMain)
    project.addComponent(classOf[Depends], depends)

    // add a Java component with all our classpath info
    val _targetDir = targetDir
    val java = new JavaComponent(project) {
      override def classes = Seq(classesDir)
      override def targetDir = _targetDir
      override def outputDir = classesDir
      override def buildClasspath :SeqV[Path] =
        classesDir +: (if (isMain) depends.buildClasspath
                       else mainOutputDir +: depends.testClasspath)
      override def execClasspath :SeqV[Path] = classesDir +: depends.execClasspath
    }
    project.addComponent(classOf[JavaComponent], java)
    java.addTesters()

    def allLangs (java :Path) :Seq[Path] = {
      // if the java path is not of the form foo/java then we can't langify it
      if (java.getFileName.toString != "java") Seq(java)
      // otherwise turn foo/java into foo/scala, etc.
      else (Seq(java) ++ Seq("scala", "kotlin", "kt").map(java.getParent.resolve(_))).
        filter(Files.exists(_))
    }

    // add a sources component with our source directories
    val sources = new Sources(
      if (isMain) allLangs(buildDir("sourceDirectory", "src/main/java"))
      else allLangs(buildDir("testSourceDirectory", "src/test/java")))
    project.addComponent(classOf[Sources], sources)

    def readJavacOpts :Seq[String] = {
      // this returns 0-N Plugin instances (one for each POM in the parent chain)
      val cps  = pom.plugin("org.apache.maven.plugins", "maven-compiler-plugin")
      // look for source/target configuration; if this is a test project we need to also consider
      // 'testSource' and 'testTarget' and since these values can be defined in this or a parent POM
      // there's some ambiguity as to which we should prefer, but we just punt and prefer 'testFoo'
      // regardless of where it's specified and only fall back to 'foo' if no 'testFoo' is explicitly
      // provided; yay Maven
      def versValue(id :String) = cps.flatMap(_.configValue(id)).takeRight(1).fromScala
      def modeVersValue(id :String) = if (isMain) versValue(id) else {
        val testId = "test" + id.charAt(0).toUpper + id.substring(1)
        val testValue = versValue(testId)
        if (testValue.isEmpty) versValue(id) else testValue
      }
      modeVersValue("source").flatMap(List("-source", _)) ++
        modeVersValue("target").flatMap(List("-target", _)) ++
        // also look for <compilerArgs> sections
        cps.flatMap(_.configList("compilerArgs", "arg")).fromScala
    }

    def readScalacOpts :Seq[String] = {
      // look for info from either maven-scala or scala-maven plugins
      val msp = pom.plugin("org.scala-tools", "maven-scala-plugin")
      val smp = pom.plugin("net.alchim31.maven", "scala-maven-plugin")
      // the above returns info for every POM up the chain of parents, so we flatten any config
      // directives we find therein into one
      (if (msp.isEmpty) smp else msp).flatMap(_.configList("args", "arg")).fromScala
    }

    def readKotlincOpts :Seq[String] = {
      // this above returns info for every POM up the chain of parents, so we flatten any config
      // directives we find therein into one
      pom.plugin("org.jetbrains.kotlin", "kotlin-maven-plugin").
        flatMap(_.configList("args", "arg")).fromScala
    }

    def copyResources () {
      (if (isMain) pom.resources else pom.testResources) foreach { rsrc =>
        val target = classesDir
        if (rsrc.targetPath.isDefined || rsrc.filtering || !rsrc.includes.isEmpty ||
            !rsrc.excludes.isEmpty) project.log("Complex <resources> not yet supported " + rsrc)
        else {
          val rsrcDir = rootPath.resolve(rsrc.directory)
          if (Files.exists(rsrcDir)) Filez.copyAll(rsrcDir, target)
        }
      }
    }

    // TODO: this is expensive, can we do something cheaper?
    val ssum = sources.summarize
    // TODO: do we want to try to support multi-lingual projects? that sounds like a giant PITA,
    // but we could probably at least generate a warning if we have some crazy mishmash of sources

    // TEMP: if we have any Kotlin files, we just use the KotlinCompiler
    if (ssum.contains("kt")) {
      project.addComponent(classOf[Compiler], new KotlinCompiler(project, java) {
        // this will be the same for main and test projects, putting them in the same module;
        // TODO: except it doesn't work and seems to cause other problems; sigh override
        // def moduleName = Some(pom.artifactId)
        override def javacOpts = readJavacOpts
        override def kotlincOpts = readKotlincOpts
        override def kotlincVers = depends.artifactVers(
          "org.jetbrains.kotlin", "kotlin-stdlib", super.kotlincVers)
        override protected def willCompile () = copyResources()
      })

    } else {
      project.addComponent(classOf[Compiler], new ScalaCompiler(project, java) {
        override def javacOpts = readJavacOpts
        override def scalacOpts = readScalacOpts
        override def scalacVers = depends.artifactVers(
          "org.scala-lang", "scala-library", super.scalacVers)
        override protected def willCompile () = copyResources()
      })
    }

    val name = projName(isMain)
    val ids = Set.builder[Id]()
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
    val testRoot = if (isMain) Some(Root(rootPath, "test")) else None
    project.metaV() = Meta(name, ids.build(), testRoot)
  }
}
