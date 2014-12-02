//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// http://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path}
import pomutil.POM
import scaled._
import scaled.pacman.Filez

class MavenProject (val root :Project.Root, ps :ProjectSpace) extends AbstractJavaProject(ps) {
  import Project._

  private def isMain = !root.testMode
  private def name (isMain :Boolean) = pom.artifactId + (if (isMain) "" else "-test")

  private val pomFile = root.path.resolve("pom.xml")
  private var _pom = POM.fromFile(pomFile.toFile) getOrElse {
    throw new IllegalArgumentException(s"Unable to load $pomFile")
  }
  def pom = _pom

  // watch the POM file for changes, if it does change, hibernate which will cause is to reload
  // everything when we're next referenced
  metaSvc.service[WatchService].watchFile(pomFile, file => POM.fromFile(file.toFile) match {
    case SNone =>
      metaSvc.log.log(s"$name: auto-reload failed: $file")
    case SSome(pom) =>
      metaSvc.log.log(s"$name: auto-reloded: $file")
      hibernate()
      _pom = pom
  })
  // note that we don't 'close' our watch, we'll keep it active for the lifetime of the editor
  // because it's low overhead; I may change my mind on this front later, hence this note

  override def name = name(isMain)
  override def idName = s"mvn-${pom.groupId}_${name}_${pom.version}"
  override def ids = Seq(RepoId(MavenRepo, pom.groupId, pom.artifactId, pom.version)) ++ pomSrcId
  override def testSeed = if (!isMain) None else {
    val troot = Project.Root(root.path, true)
    Some(Project.Seed(troot, name(false), true, getClass, List(troot)))
  }
  override def depends = _depends.transitive :+ _depends.platformDepend

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

  private def mainOutputDir = buildDir("outputDirectory", s"$targetPre/classes")
  private def testOutputDir = buildDir("testOutputDirectory", s"$targetPre/test-classes")
  override def outputDir :Path = if (isMain) mainOutputDir else testOutputDir

  // TODO: use summarizeSources to determine whether to use a Java or Scala compiler
  override protected def createCompiler () = new ScalaCompiler(this) {
    override def sourceDirs = MavenProject.this.sourceDirs
    override def buildClasspath = MavenProject.this.buildClasspath
    override def outputDir = MavenProject.this.outputDir

    override def javacOpts = {
      // look for source/target configuration
      val cps = pom.plugin("org.apache.maven.plugins", "maven-compiler-plugin")
      cps.flatMap(_.configValue("source")).takeRight(1).fromScala.flatMap(List("-source", _)) ++
      cps.flatMap(_.configValue("target")).takeRight(1).fromScala.flatMap(List("-target", _))
    }
    // override def scalacOpts :Seq[String] = Seq()

    override protected def willCompile () {
      pom.resources foreach copyResources(outputDir)
    }
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