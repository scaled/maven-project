//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// http://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import codex.extract.JavaExtractor
import codex.model.Source
import java.io.File
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile
import pomutil.POM
import scaled._

class MavenArtifactProject (af :MavenArtifactProject.Artifact, ps :ProjectSpace)
    extends AbstractZipFileProject(ps) with JavaProject {

  private def id = af.repoId
  private val _pom = POM.fromFile(af.pom.toFile) getOrElse {
    throw new IllegalArgumentException("Unable to load ${af.pom}")
  }
  private val _depends = new Depends(pspace) {
    def pom = _pom
  }

  override val root = af.sources
  override val zipPaths = Seq(af.sources)
  override def isIncidental = true
  override def name = s"${id.artifactId}:${id.version}"
  override def idName = s"mvn-${id.groupId}_${id.artifactId}_${id.version}"
  override def ids = Seq(id)
  override def classes = af.classes
  override def depends = _depends.transitive :+ _depends.platformDepend

  override protected def createProjectCodex () :ProjectCodex = new ProjectCodex(this) {
    import scala.collection.convert.WrapAsJava._

    val javac = new JavaExtractor() {
      override def classpath = _depends.buildClasspath
    }

    // if our project store is empty, run an initial index immediately
    if (!projectStore.topLevelDefs.iterator.hasNext) {
      // (TODO: queue this up so that we're only indexing one project at a time?)
      metaSvc.exec.runInBG { reindexAll() }
    }

    // TODO: use Nexus or actors instead of this ham-fisted syncing
    def reindexAll () :Unit = synchronized {
      val sources = new ZipFile(root.toFile)
      println(s"Reindexing ${sources.size} java files in $root...")
      javac.process(sources, projectStore.writer)
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
    private def isJar (path :Path) = path.getFileName.toString endsWith ".jar"
    private def isInM2Repo (path :Path) = path startsWith Maven.m2repo
    override def checkRoot (root :Path) :Int = if (isJar(root) && isInM2Repo(root)) 1 else -1

    override def apply (id :Project.Id) = id match {
      case repoId :RepoId =>
        val af = Artifact(repoId)
        if (Files.exists(af.sources)) Some(seed(af.sources, af :: Nil))
        else None
      case _ => None
    }

    private def rootToArtifact (root :Path) :Artifact = {
      val versDir = root.getParent ; val version = versDir.getFileName.toString
      val artiDir = versDir.getParent ; val artifactId = artiDir.getFileName.toString
      val groupPath = Maven.m2repo.relativize(artiDir.getParent)
      val groupId = groupPath.toString.replace(File.separatorChar, '.')
      Artifact(RepoId(MavenRepo, groupId, artifactId, version))
    }
    override protected def injectArgs (root :Path) = rootToArtifact(root) :: Nil
  }
}
