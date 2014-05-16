//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// http://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import java.io.File
import pomutil.{DependResolver, Dependency, POM}
import reactual.Future
import scaled._

class MavenProject (
  root :File, log :Logger, exec :Executor, metaSvc :MetaService, projectSvc :ProjectService
) extends FileProject(root, log, metaSvc) {

  private[this] val pomFile = new File(root, "pom.xml")
  // TODO: reload the POM if it changes? restart the compiler if so...
  private[this] var pom = POM.fromFile(pomFile) getOrElse {
    throw new IllegalArgumentException("Unable to load $pomFile")
  }

  override def name = pom.artifactId
  override def id = Some(pom.id)
  override def sourceURL = pom.scm.connection.map(stripSCM)

  // TODO: figure out what kind of compiler we should use based on what?
  //       plugins in POM, source files, chicken sacrifice?
  override protected def createCompiler () = new ScalaCompiler(this, exec, log)
  override protected def ignores = MavenProject.mavenIgnores

  def sourceDirs :Seq[File] = Seq(buildDir("sourceDirectory", "src/main"))
  def testSourceDirs :Seq[File] = Seq(buildDir("testSourceDirectory", "src/test"))

  def outputDir :File = buildDir("outputDirectory", "target/classes")
  def testOutputDir :File = buildDir("testOutputDirectory", "target/test-classes")

  def buildClasspath :Seq[File] = outputDir +: classpath(transitiveDepends(false))
  def testClasspath :Seq[File] = testOutputDir +: classpath(transitiveDepends(true))

  private def file (root :File, comps :String*) :File = (root /: comps)(new File(_, _))

  private def buildDir (key :String, defpath :String) :File =
    file(root, pom.buildProps.getOrElse(key, defpath).split("/") :_*)

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
        if (!m2file.isDefined) log.log(s"MavenProject($root) unable to resolve jar for $dep")
        m2file
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
