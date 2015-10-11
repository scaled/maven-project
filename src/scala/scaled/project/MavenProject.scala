//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// http://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path}
import pomutil.POM
import scaled._
import scaled.pacman.Filez
import scaled.util.BufferBuilder

class MavenProject (val root :Project.Root, ps :ProjectSpace) extends AbstractJavaProject(ps) {
  import Project._

  private def isMain = !root.testMode
  private def name (isMain :Boolean) = pom.artifactId + (if (isMain) "" else "-test")

  private val pomFile = root.path.resolve("pom.xml")
  private var _pom = POM.fromFile(pomFile.toFile) getOrElse {
    throw new IllegalArgumentException(s"Unable to load $pomFile")
  }
  def pom = _pom

  // watch the POM file and any local parents for changes
  watchPOM(pom)
  private def watchPOM (pom :POM) :Unit = {
    def reload (unused :Path) = POM.fromFile(pomFile.toFile) match {
      case SNone =>
        metaSvc.log.log(s"$name: auto-reload failed: $pomFile")
      case SSome(pom) =>
        metaSvc.log.log(s"$name: auto-reloded: $pomFile")
        hibernate()
        _pom = pom
    }
    def watch (file :Path) = { metaSvc.service[WatchService].watchFile(file, reload) }
    pom.file foreach { f => watch(f.toPath) }
    pom.parent foreach { watchPOM }
  }
  // note that we don't 'close' our watches, we'll keep them active for the lifetime of the editor
  // because it's low overhead; I may change my mind on this front later, hence this note

  override def name = name(isMain)
  override def idName = s"mvn-${pom.groupId}_${name}_${pom.version}"
  override def ids = {
    val ids = Seq.builder[Id]()
    // TODO: we could have our RepoId support a classifier and have our id be classified as tests
    // for the test sub-project, but for now we just use artifactId-test
    ids += RepoId(MavenRepo, pom.groupId, name, pom.version)
    if (isMain) ids ++= pomSrcId
    ids.build()
  }
  override def testSeed = if (!isMain) None else {
    val troot = Project.Root(root.path, true)
    Some(Project.Seed(troot, name(false), true, getClass, List(troot)))
  }
  override def depends = {
    val deps = Seq.builder[Id]
    // if this is the test subproject, add a depend on the main project
    if (!isMain) deps += RepoId(MavenRepo, pom.groupId, pom.artifactId, pom.version)
    deps ++= _depends.transitive
    deps += _depends.platformDepend
    deps.build()
  }

  private def pomSrcId = {
    def stripSCM (url :String) = if (url startsWith "scm:") url.substring(4) else url
    Option.from(pom.scm.connection) map(stripSCM(_).split(":", 2)) collect {
      case Array(vcs, url) => SrcURL(vcs, url)
    }
  }

  private def mainSourceDirs = allLangs(buildDir("sourceDirectory", "src/main/java"))
  private def testSourceDirs = allLangs(buildDir("testSourceDirectory", "src/test/java"))
  override def sourceDirs :Seq[Path] = if (isMain) mainSourceDirs else testSourceDirs

  private def allLangs (java :Path) :Seq[Path] = {
    // if the java path is not of the form foo/java then we can't langify it
    if (java.getFileName.toString != "java") Seq(java)
    // otherwise turn foo/java into foo/scala, etc.
    else (Seq(java) ++ Seq("scala").map(java.getParent.resolve(_))).filter(Files.exists(_))
  }

  private def targetPre = pom.buildProps.getOrElse("directory", "target")
  private def targetDir = buildDir("directory", "target")

  override protected def describeBuild (bb :BufferBuilder) {
    super.describeBuild(bb)

    bb.addSection("Compiler options:")
    bb.addKeyValue("javac: ", javacOpts.mkString(" "))
    bb.addKeyValue("scalac: ", scalacOpts.mkString(" "))
  }

  private def mainOutputDir = buildDir("outputDirectory", s"$targetPre/classes")
  private def testOutputDir = buildDir("testOutputDirectory", s"$targetPre/test-classes")
  override def outputDir :Path = if (isMain) mainOutputDir else testOutputDir

  // TODO: use summarizeSources to determine whether to use a Java or Scala compiler
  override protected def createCompiler () = new ScalaCompiler(this) {
    override def sourceDirs = MavenProject.this.sourceDirs
    override def buildClasspath = MavenProject.this.buildClasspath
    override def outputDir = MavenProject.this.outputDir

    override def javacOpts = MavenProject.this.javacOpts
    override def scalacOpts :Seq[String] = MavenProject.this.scalacOpts

    override protected def willCompile () {
      (if (isMain) pom.resources else pom.testResources) foreach copyResources(outputDir)
    }
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
    // this returns 0-N Plugin instances (one for each POM in the parent chain)
    val cps  = pom.plugin("org.scala-tools", "maven-scala-plugin")
    // also look for <configuration>/<args> sections
    cps.flatMap(_.configList("args", "arg")).fromScala
  }

  private def copyResources (target :Path)(rsrc :POM.Resource) {
    if (rsrc.targetPath.isDefined || rsrc.filtering || !rsrc.includes.isEmpty ||
        !rsrc.excludes.isEmpty) metaSvc.log.log("Complex <resources> not yet supported " + rsrc)
    else {
      val rsrcDir = root.path.resolve(rsrc.directory)
      if (Files.exists(rsrcDir)) Filez.copyAll(rsrcDir, target)
    }
  }

  override protected def ignore (dir :Path) :Boolean = {
    super.ignore(dir) || (dir == targetDir) || (dir == outputDir)
  }

  override protected def buildDependClasspath =
    if (isMain) _depends.buildClasspath else mainOutputDir +: _depends.testClasspath
  override protected def execDependClasspath = _depends.execClasspath

  private val _depends = new Depends(pspace) {
    def pom = _pom
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
      Project.Root(path, ppath.exists(_.getFileName.toString == "test"))
    }
  }
}
