//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// http://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path}
import pomutil.POM
import scaled._
import scaled.pacman.Filez

class MavenProject (val root :Path, ps :ProjectSpace) extends AbstractJavaProject(ps) {
  import Project._

  private val pomFile = root.resolve("pom.xml")
  private var _pom = POM.fromFile(pomFile.toFile) getOrElse {
    throw new IllegalArgumentException("Unable to load $pomFile")
  }
  def pom = _pom

  // watch the POM file for changes, if it does change, hibernate which will cause is to reload
  // everything when we're next referenced
  metaSvc.service[WatchService].watchFile(pomFile, file => {
    POM.fromFile(file.toFile) match {
      case None =>
        metaSvc.log.log(s"$name: auto-reload failed: $file")
      case Some(pom) =>
        metaSvc.log.log(s"$name: auto-reloded: $file")
        hibernate()
        _pom = pom
    }
  })
  // note that we don't 'close' our watch, we'll keep it active for the lifetime of the editor
  // because it's low overhead; I may change my mind on this front later, hence this note

  override def name = pom.artifactId
  override def idName = s"mvn-${pom.groupId}_${pom.artifactId}_${pom.version}"
  override def ids = Seq(RepoId(MavenRepo, pom.groupId, pom.artifactId, pom.version)) ++ pomSrcId
  override def depends = _depends.transitive :+ _depends.platformDepend

  private def pomSrcId = {
    def stripSCM (url :String) = if (url startsWith "scm:") url.substring(4) else url
    pom.scm.connection map(stripSCM(_).split(":", 2)) collect {
      case Array(vcs, url) => SrcURL(vcs, url)
    }
  }

  override def sourceDirs :Seq[Path] = Seq(buildDir("sourceDirectory", "src/main"))
  override def testSourceDirs :Seq[Path] = Seq(buildDir("testSourceDirectory", "src/test"))

  override def outputDir :Path = buildDir("outputDirectory", "target/classes")
  override def testOutputDir :Path = buildDir("testOutputDirectory", "target/test-classes")

  // TODO: use summarizeSources to determine whether to use a Java or Scala compiler
  override protected def createCompiler () = new ScalaCompiler(this) {
    override def sourceDirs = MavenProject.this.sourceDirs
    override def buildClasspath = MavenProject.this.buildClasspath
    override def outputDir = MavenProject.this.outputDir
    override def testSourceDirs = MavenProject.this.testSourceDirs
    override def testClasspath = MavenProject.this.testClasspath
    override def testOutputDir = MavenProject.this.testOutputDir

    override def javacOpts = {
      // look for source/target configuration
      val cps = pom.plugin("org.apache.maven.plugins", "maven-compiler-plugin")
      cps.flatMap(_.configValue("source")).takeRight(1).flatMap(List("-source", _)) ++
      cps.flatMap(_.configValue("target")).takeRight(1).flatMap(List("-target", _))
    }
    // override def scalacOpts :Seq[String] = Seq()

    override protected def willCompile (tests :Boolean) {
      if (tests) pom.testResources foreach copyResources(testOutputDir)
      else pom.resources foreach copyResources(outputDir)
    }
  }

  private def copyResources (target :Path)(rsrc :POM.Resource) {
    if (rsrc.targetPath.isDefined || rsrc.filtering || !rsrc.includes.isEmpty ||
        !rsrc.excludes.isEmpty) metaSvc.log.log("Complex <resources> not yet supported " + rsrc)
    else {
      val rsrcDir = root.resolve(rsrc.directory)
      if (Files.exists(rsrcDir)) Filez.copyAll(rsrcDir, target)
    }
  }

  override protected def ignores = MavenProject.mavenIgnores
  override protected def buildDependClasspath = _depends.buildClasspath
  override protected def testDependClasspath = _depends.testClasspath
  override protected def execDependClasspath = _depends.execClasspath

  private val _depends = new Depends(pspace) {
    def pom = _pom
  }
  private def buildDir (key :String, defpath :String) :Path =
    root.resolve(pom.buildProps.getOrElse(key, defpath))
}

object MavenProject {

  // TODO: don't do things this way, determine the classes directory from the POM, etc.
  val mavenIgnores = FileProject.stockIgnores ++ Set("target")

  @Plugin(tag="project-finder")
  class FinderPlugin extends ProjectFinderPlugin("maven", true, classOf[MavenProject]) {
    def checkRoot (root :Path) :Int = if (exists(root, "pom.xml")) 1 else -1
  }
}
