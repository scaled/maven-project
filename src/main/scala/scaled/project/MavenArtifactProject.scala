//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// http://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import codex.extract.JavaExtractor
import codex.model.Source
import codex.store.EphemeralStore
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile
import pomutil.POM
import scaled._

class MavenArtifactProject (af :MavenArtifactProject.Artifact, msvc :MetaService)
    extends AbstractZipFileProject(msvc) with JavaProject {

  private def id = af.repoId
  private val _pom = POM.fromFile(af.pom.toFile) getOrElse {
    throw new IllegalArgumentException("Unable to load ${af.pom}")
  }
  private val _depends = new Depends(metaSvc.service[ProjectService]) {
    def pom = _pom
  }

  override val root = af.sources
  override val zipPaths = Seq(af.sources)
  override def isIncidental = true
  override def name = s"${id.artifactId}:${id.version}"
  override def ids = Seq(id)
  override def classes = af.classes

  override def depends = _depends.transitive(false) :+ _depends.platformDepend

  override protected def metaDir = af.pom.getParent.resolve(".scaled")

  // TEMP: for now auto-populate project store the first time we're loaded
  override protected def createProjectCodex () :ProjectCodex = new ProjectCodex(this) {
    import scala.collection.convert.WrapAsJava._

    val javac = new JavaExtractor() {
      override def classpath = _depends.classpath(false)
    }
    def estore = projectStore.asInstanceOf[EphemeralStore]

    // the first time we're run, compile all of our source files
    metaSvc.exec.runInBG { reindexAll() }

    // TODO: use Nexus or actors instead of this ham-fisted syncing
    def reindexAll () :Unit = synchronized {
      val sources = new ZipFile(zipPaths.head.toFile)
      println(s"Reindexing ${sources.size} java files in $zipPaths...")
      javac.process(sources, estore.writer)
    }

    override protected def reindex (source :Source) :Unit = synchronized {
      reindexComplete(source) // our metadata is always up to date
    }
  }

  // TODO: try to download our -sources file if it does not already exist
}

object MavenArtifactProject {
  import Project._
  import Maven._

  case class Artifact (repoId :RepoId) {
    lazy val pom     = resolvePOM(repoId)
    lazy val classes = resolveClasses(repoId)
    lazy val sources = resolveSources(repoId)
  }

  @Plugin(tag="project-finder")
  class FinderPlugin extends ProjectFinderPlugin("mavenrepo", true, classOf[MavenArtifactProject]) {
    override def checkRoot (root :Path) :Int = -1 // we never resolve by root

    override def apply (id :Project.Id) = id match {
      case repoId :RepoId =>
        val af = Artifact(repoId)
        if (Files.exists(af.sources)) Some(createProject(af))
        else None
      case _ => None
    }
  }
}
