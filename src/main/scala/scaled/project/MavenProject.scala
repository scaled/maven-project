//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// http://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import codex.extract.JavaExtractor
import codex.store.EphemeralStore
import codex.store.ProjectStore
import com.google.common.collect.{Multimap, HashMultimap}
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
  import Project._

  private[this] val pomFile = root.resolve("pom.xml")
  private[this] var pom = POM.fromFile(pomFile.toFile) getOrElse {
    throw new IllegalArgumentException("Unable to load $pomFile")
  }

  // watch the POM file for changes, if it does change, hibernate which will cause is to reload
  // everything when we're next referenced
  metaSvc.service[WatchService].watchFile(pomFile, file => {
    POM.fromFile(file.toFile) match {
      case None =>
        metaSvc.log.log(s"$name: auto-reload failed: $file")
      case Some(pom) =>
        metaSvc.log.log(s"$name: auto-reloded: $file")
        hibernate()
        this.pom = pom
    }
  })
  // note that we don't 'close' our watch, we'll keep it active for the lifetime of the editor
  // because it's low overhead; I may change my mind on this front later, hence this note

  override def name = pom.artifactId
  override def id = Some(RepoId(MavenRepo, pom.groupId, pom.artifactId, pom.version))
  override def sourceURL = pom.scm.connection.map(stripSCM).flatMap(srcURLFromString)
  override def depends = transitiveDepends(false) map { dep =>
    RepoDepend(RepoId(MavenRepo, dep.groupId, dep.artifactId, dep.version))
  }

  override def describeSelf (bb :BufferBuilder) {
    super.describeSelf(bb)

    bb.addSubHeader("Maven Info")
    bb.addSection("Source dirs:")
    bb.addKeysValues("compile: " -> sourceDirs.mkString(" "),
                     "test: "    -> testSourceDirs.mkString(" "))
    val srcsum = summarizeSources(true)
    if (!srcsum.isEmpty) {
      bb.addSection("Source files:")
      bb.addKeysValues(srcsum.asMap.entrySet.map(
        e => (s".${e.getKey}: ", e.getValue.size.toString)).toSeq :_*)
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
    override def sourceDirs = MavenProject.this.sourceDirs
    override def buildClasspath = MavenProject.this.buildClasspath
    override def outputDir = MavenProject.this.outputDir
    override def testSourceDirs = MavenProject.this.testSourceDirs
    override def testClasspath = MavenProject.this.testClasspath
    override def testOutputDir = MavenProject.this.testOutputDir
  }

  // TODO: how to determine what kind of tester to use?
  override protected def createTester () :Tester = new JUnitTester(this) {
    override def testSourceDirs = MavenProject.this.testSourceDirs
    override def testOutputDir = MavenProject.this.testOutputDir
    override def testClasspath = MavenProject.this.testClasspath
  }

  override protected def createRunner () = new JavaRunner(this) {
    override def execClasspath = buildClasspath // TODO
  }

  override protected def ignores = MavenProject.mavenIgnores

  // TEMP: for now auto-populate project store the first time we're loaded
  override protected def createProjectStore () :ProjectStore = {
    import scala.collection.convert.WrapAsJava._
    val store = super.createProjectStore().asInstanceOf[EphemeralStore]
    val javas = summarizeSources(false).get("java")
    if (!javas.isEmpty) {
      new JavaExtractor() {
        override def classpath = buildClasspath
      }.process(javas, store.writer)
    }
    store
  }

  def summarizeSources (includeTest :Boolean) :Multimap[String,Path] = {
    val bySuff = HashMultimap.create[String,Path]()
    onSources(includeTest) { file =>
      val fname = file.getFileName.toString
      fname.lastIndexOf(".") match {
        case -1 => // skip it!
        case ii => bySuff.put(fname.substring(ii+1), file)
      }
    }
    bySuff
  }

  private def onSources (includeTest :Boolean)(fn :Path => Unit) {
    (if (includeTest) allSourceDirs else sourceDirs).filter(Files.exists(_)) foreach { dir =>
      // TODO: should we be following symlinks? likely so...
      Files.walkFileTree(dir, new SimpleFileVisitor[Path]() {
        override def visitFile (file :Path, attrs :BasicFileAttributes) = {
          if (!attrs.isDirectory) fn(file)
          FileVisitResult.CONTINUE
        }
      })
    }
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
    // check whether this dependency is known to Scaled, and use the known version instead of the
    // default (which looks in ~/.m2 for an installed POM)
    override def localDep (dep :Dependency) = (for {
      proj <- projectSvc.projectFor(toDepend(dep))
      pom  <- POM.fromFile(proj.root.resolve("pom.xml").toFile)
    } yield pom) orElse super.localDep(dep)
  }.resolve(forTest)

  protected def classpath (deps :Seq[Dependency]) :Seq[Path] = deps flatMap { dep =>
    projectSvc.projectFor(toDepend(dep)) match {
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

  private def toDepend (dep :Dependency) =
    RepoDepend(RepoId(MavenRepo, dep.groupId, dep.artifactId, dep.version))
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
