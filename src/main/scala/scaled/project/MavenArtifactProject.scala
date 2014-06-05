//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// http://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path}
import scaled._

class MavenArtifactProject (af :MavenArtifactProject.Artifact, msvc :MetaService)
    extends AbstractZipFileProject(msvc) with JavaProject {

  def id = af.repoId

  override val root = af.pom
  override val zipPaths = if (Files.exists(af.sources)) Seq(af.sources) else Seq()
  override def isIncidental = true
  override def name = s"${id.artifactId}:${id.version}"
  override def ids = Seq(id)
  override def classes = af.classes

  // TODO: project store on our -sources jar

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
        if (Files.exists(af.classes)) Some(createProject(af))
        else None
      case _ => None
    }
  }
}
