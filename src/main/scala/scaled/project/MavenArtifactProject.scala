//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// http://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path}
import scaled._

class MavenArtifactProject (artifact :MavenArtifactProject.Artifact, metaSvc :MetaService)
    extends ZipFileProject(artifact.root, metaSvc) with JavaProject {

  def id = artifact.repoId
  override def isIncidental = true
  override def name = s"${id.artifactId}:${id.version}"
  override def ids = Seq(id)

  // our classes are in our root jar
  override def classes = artifact.classes
}

object MavenArtifactProject {
  import Project._
  import Maven._

  case class Artifact (repoId :RepoId) {
    val pom     = resolvePOM(repoId)
    val classes = resolveClasses(repoId)
    val sources = resolveSources(repoId)

    // if we have a classes jar file for this dependency, go for it
    def validate :Option[Artifact] = if (Files.exists(classes)) Some(this) else None

    // we'll use sources if we have 'em but otherwise we still try to do something useful
    def root = if (Files.exists(sources)) sources else classes
  }

  @Plugin(tag="project-finder")
  class FinderPlugin extends ProjectFinderPlugin("mavenrepo", true, classOf[MavenArtifactProject]) {
    override def checkRoot (root :Path) :Int = -1 // we never resolve by root

    override def apply (id :Project.Id) = id match {
      case repoId :RepoId => Artifact(repoId).validate.map(createProject(_))
      case _ => None
    }
  }
}
