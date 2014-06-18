//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// http://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import codex.extract.JavaExtractor
import codex.model.Source
import codex.store.ProjectStore
import com.google.common.collect.{Multimap, HashMultimap}
import java.nio.file.Paths
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, FileVisitResult, Path, SimpleFileVisitor}
import pomutil.{DependResolver, Dependency, POM}
import reactual.Future
import scala.collection.mutable.{Map => MMap}
import scaled._
import scaled.util.BufferBuilder

class MavenProject (val root :Path, msvc :MetaService, projectSvc :ProjectService)
    extends AbstractFileProject(msvc) with JavaProject {
  import scala.collection.convert.WrapAsScala._
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

  def sourceDirs :Seq[Path] = Seq(buildDir("sourceDirectory", "src/main"))
  def testSourceDirs :Seq[Path] = Seq(buildDir("testSourceDirectory", "src/test"))
  def allSourceDirs = sourceDirs ++ testSourceDirs

  def outputDir :Path = buildDir("outputDirectory", "target/classes")
  def testOutputDir :Path = buildDir("testOutputDirectory", "target/test-classes")

  def buildClasspath :Seq[Path] = outputDir +: _depends.classpath(false)
  def testClasspath :Seq[Path] = testOutputDir +: outputDir +: _depends.classpath(true)

  override def name = pom.artifactId

  override def ids = Seq(RepoId(MavenRepo, pom.groupId, pom.artifactId, pom.version)) ++ {
    def stripSCM (url :String) = if (url startsWith "scm:") url.substring(4) else url
    pom.scm.connection map(stripSCM(_).split(":", 2)) collect {
      case Array(vcs, url) => SrcURL(vcs, url)
    }
  }

  override def depends = _depends.transitive(false) :+ _depends.platformDepend

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

  // tell other Java projects where to find our compiled classes
  override def classes = outputDir

  // TODO: use summarizeSources to determine whether to use a Java or Scala compiler
  override protected def createCompiler () = new ScalaCompiler(this) {
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
  override protected def createProjectCodex () :ProjectCodex = new ProjectCodex(this) {
    import scala.collection.convert.WrapAsJava._

    val javac = new JavaExtractor() {
      override def classpath = buildClasspath
    }

    // the first time we're run, compile all of our source files
    // metaSvc.exec.runInBG { reindexAll() }

    // TODO: use Nexus or actors instead of this ham-fisted syncing
    def reindexAll () :Unit = synchronized {
      val javas = summarizeSources(false).get("java")
      println(s"Reindexing ${javas.size} java files in $name")
      if (!javas.isEmpty) javac.process(javas, projectStore.writer)
    }

    override protected def reindex (source :Source) :Unit = synchronized {
      if (source.fileExt == "java") {
        println(s"Reindexing $source")
        // TODO: have JavaExtractor take Source and make a JavaFileObject from it?
        // also TODO: only reindex if the file has changed since we last indexed
        javac.process(Seq(Paths.get(source.toString)), projectStore.writer)
      }
      reindexComplete(source)
    }
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

  private val _depends = new Depends(projectSvc) {
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
