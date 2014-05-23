//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// http://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import com.google.common.collect.{Multiset, HashMultiset}
import java.io.File
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, FileVisitResult, Path, SimpleFileVisitor}
import pomutil.{DependResolver, Dependency, POM}
import reactual.Future
import scala.collection.mutable.{Map => MMap}
import scaled._
import scaled.util.BufferBuilder

class MavenProject (root :Path, metaSvc :MetaService, projectSvc :ProjectService)
    extends FileProject(root, metaSvc) {
  import scala.collection.convert.WrapAsScala._

  private[this] val pomFile = root.resolve("pom.xml")
  // TODO: reload the POM if it changes? restart the compiler if so...
  private[this] var pom = POM.fromFile(pomFile.toFile) getOrElse {
    throw new IllegalArgumentException("Unable to load $pomFile")
  }

  override def name = pom.artifactId
  override def id = Some(pom.id)
  override def sourceURL = pom.scm.connection.map(stripSCM)

  override def describeSelf (bb :BufferBuilder) {
    super.describeSelf(bb)

    bb.addSubHeader("Maven Info")
    bb.addSection("Source dirs:")
    bb.addKeysValues("compile: " -> sourceDirs.mkString(" "),
                     "test: "    -> testSourceDirs.mkString(" "))
    val srcsum = summarizeSources
    if (!srcsum.isEmpty) {
      bb.addSection("Source files:")
      bb.addKeysValues(srcsum.entrySet.map(
        e => (s"${e.getElement}: ", e.getCount.toString)).toSeq :_*)
    }
    bb.addSection("Output dirs:")
    bb.addKeysValues("compile: " -> outputDir.toString,
                     "test: "    -> testOutputDir.toString)
    bb.addSection("Compile classpath:")
    buildClasspath foreach { p => bb.add(p.toString) }
    bb.addSection("Test classpath:")
    testClasspath foreach { p => bb.add(p.toString) }
  }

  // TODO: use summarizeSources to determine whether to use a Java or Scala compiler
  override protected def createCompiler () = new ScalaCompiler(metaSvc, this) {
    def sourceDirs = MavenProject.this.sourceDirs
    def buildClasspath = MavenProject.this.buildClasspath
    def outputDir = MavenProject.this.outputDir
    def testSourceDirs = MavenProject.this.testSourceDirs
    def testClasspath = MavenProject.this.testClasspath
    def testOutputDir = MavenProject.this.testOutputDir
  }

  override protected def createRunner () = new JavaRunner(this) {
    override def execClasspath = buildClasspath // TODO
  }

  override protected def ignores = MavenProject.mavenIgnores

  def summarizeSources :Multiset[String] = {
    val counts = HashMultiset.create[String](2)
    allSourceDirs.filter(Files.exists(_)) foreach { dir =>
      // TODO: should we be following symlinks? likely so...
      Files.walkFileTree(dir, new SimpleFileVisitor[Path]() {
        override def visitFile (file :Path, attrs :BasicFileAttributes) = {
          if (!attrs.isDirectory) {
            val fname = file.getFileName.toString
            if (fname endsWith ".java") counts.add(".java")
            else if (fname endsWith ".scala") counts.add(".scala")
          }
          FileVisitResult.CONTINUE
        }
      })
    }
    counts
  }

  def sourceDirs :Seq[Path] = Seq(buildDir("sourceDirectory", "src/main"))
  def testSourceDirs :Seq[Path] = Seq(buildDir("testSourceDirectory", "src/test"))
  def allSourceDirs = sourceDirs ++ testSourceDirs

  def outputDir :Path = buildDir("outputDirectory", "target/classes")
  def testOutputDir :Path = buildDir("testOutputDirectory", "target/test-classes")

  def buildClasspath :Seq[Path] = outputDir +: classpath(transitiveDepends(false))
  def testClasspath :Seq[Path] = testOutputDir +: outputDir +: classpath(transitiveDepends(true))

  private def buildDir (key :String, defpath :String) :Path =
    root.resolve(pom.buildProps.getOrElse(key, defpath))

  protected def transitiveDepends (forTest :Boolean) :Seq[Dependency] = new DependResolver(pom) {
    // TODO: check whether this dependency is known to Scaled, and use the known version instead
    // of the default (which looks in ~/.m2 for an installed POM)
    override def localDep (dep :Dependency) = (for {
      proj <- projectSvc.projectForId(dep.id)
      pom  <- POM.fromFile(proj.root.resolve("pom.xml").toFile)
    } yield pom) orElse super.localDep(dep)
  }.resolve(forTest)

  protected def classpath (deps :Seq[Dependency]) :Seq[Path] = deps flatMap { dep =>
    projectSvc.projectForId(dep.id) match {
      case Some(proj :MavenProject) =>
        // TODO: have JavaProject which reports outputDir, so that we can interoperate
        // with non-MavenProjects?
        Some(proj.outputDir)
      case _ =>
        val m2file = dep.localArtifact orElse dep.systemPath.map(new File(_))
        if (!m2file.isDefined) log.log(s"MavenProject($root) unable to resolve jar for $dep")
        m2file.map(_.toPath)
    }
  }

  private def stripSCM (url :String) = if (url startsWith "scm:") url.substring(4) else url
}

object MavenProject {

  // TODO: don't do things this way, determine the classes directory from the POM, etc.
  val mavenIgnores = FileProject.stockIgnores ++ Set("target")

  @Plugin(tag="project-finder")
  class FinderPlugin extends ProjectFinderPlugin("maven", true, classOf[MavenProject]) {
    def checkRoot (root :Path) :Int = if (exists(root, "pom.xml")) 1 else -1
  }
}
