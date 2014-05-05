//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// http://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import java.io.File
import pomutil.{DependResolver, Dependency, POM}
import reactual.Future
import scaled._

class MavenProject (root :File, metaSvc :MetaService, projectSvc :ProjectService)
    extends FileProject(root) {

  private[this] val pomFile = new File(root, "pom.xml")
  // TODO: reload the POM if it changes? restart the compiler if so...
  private[this] var pom = POM.fromFile(pomFile) getOrElse {
    throw new IllegalArgumentException("Unable to load $pomFile")
  }

  override def name = pom.artifactId
  override def id = Some(pom.id)
  override def sourceURL = pom.scm.connection.map(stripSCM)

  override protected def createCompiler () = Some(new MavenCompiler())
  override protected def ignores = MavenProject.mavenIgnores

  def sourceDirs :Seq[File] = Seq(buildDir("sourceDirectory", "src/main"))
  def testSourceDirs :Seq[File] = Seq(buildDir("testSourceDirectory", "src/test"))

  def outputDir :File = buildDir("outputDirectory", "target/classes")
  def testOutputDir :File = buildDir("testOutputDirectory", "target/test-classes")

  private def file (root :File, comps :String*) :File = (root /: comps)(new File(_, _))

  private def buildDir (key :String, defpath :String) :File =
    file(root, pom.buildProps.getOrElse(key, defpath).split("/") :_*)

  protected def buildClasspath :Seq[File] = outputDir +: classpath(transitiveDepends(false))
  protected def testClasspath :Seq[File] = testOutputDir +: classpath(transitiveDepends(true))

  protected def transitiveDepends (forTest :Boolean) :Seq[Dependency] = new DependResolver(pom) {
    // TODO: check whether this dependency is known to Scaled, and use the known version instead
    // of the default (which looks in ~/.m2 for an installed POM)
    override def localDep (dep :Dependency) = (for {
      proj <- projectSvc.projectForId(dep.id)
      pom  <- POM.fromFile(new File(proj.root, "pom.xml"))
    } yield pom) orElse super.localDep(dep)
  }.resolve(forTest)

  protected def classpath (deps :Seq[Dependency]) :Seq[File] = deps flatMap { dep =>
    projectSvc.projectForId(dep.id) match {
      case Some(proj :MavenProject) =>
        // TODO: have JavaProject which reports outputDir, so that we can interoperate
        // with non-MavenProjects?
        Some(proj.outputDir)
      case _ =>
        val m2file = dep.localArtifact
        if (!m2file.isDefined) metaSvc.log(s"MavenProject($root) unable to resolve jar for $dep")
        m2file
    }
  }

  protected class MavenCompiler extends Compiler {
    import com.typesafe.zinc._

    val logger = new sbt.Logger {
      def trace (t : =>Throwable): Unit = metaSvc.log("trace", t)
      def success (message : =>String): Unit = metaSvc.log(message)
      def log (level :sbt.Level.Value, message : =>String): Unit = metaSvc.log(message)
    }

    val settings = Settings(
      // help: Boolean              = false,
      // version: Boolean           = false,
      // quiet: Boolean             = false,
      // logLevel: Level.Value      = Level.Info,
      color             = false,
      // sources: Seq[File]         = Seq.empty, // TODO: find all scala sources in sourceDirs
      classpath       = buildClasspath, // TODO: how/when to compile for tests
      classesDirectory = outputDir
      // scala: ScalaLocation       = ScalaLocation(),
      // scalacOptions: Seq[String] = Seq.empty,
      // javaHome: Option[File]     = None,
      // forkJava: Boolean          = false,
      // javaOnly: Boolean          = false,
      // javacOptions: Seq[String]  = Seq.empty,
      // compileOrder: CompileOrder = CompileOrder.Mixed,
      // sbt: SbtJars               = SbtJars(),
      // incOptions: IncOptions     = IncOptions(),
      // analysis: AnalysisOptions  = AnalysisOptions(),
      // analysisUtil: AnalysisUtil = AnalysisUtil(),
      // properties: Seq[String]    = Seq.empty
    )
    val zincSetup = Setup(settings)
    val compiler = Compiler(zincSetup, logger)

    def compile () = {
      val inputs = Inputs(settings)
      val vinputs = Inputs.verify(inputs)
      val cwd = Some(root)
      compiler.compile(vinputs, cwd)(logger)
      Future.success(Seq()) // TODO
    }

    def shutdown () {
      // TODO
    }
  }

  private def stripSCM (url :String) = if (url startsWith "scm:") url.substring(4) else url
}

object MavenProject {

  // TODO: don't do things this way, determine the classes directory from the POM, etc.
  val mavenIgnores = FileProject.stockIgnores ++ Set("target")

  @Plugin(tag="project-finder")
  class FinderPlugin extends ProjectFinderPlugin("maven", true, classOf[MavenProject]) {
    def checkRoot (root :File) :Int = if (new File(root, "pom.xml").exists()) 1 else -1
  }
}
