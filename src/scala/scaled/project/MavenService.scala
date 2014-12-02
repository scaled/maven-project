//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// https://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import java.nio.file.Path
import java.util.Arrays
import mfetcher.{Coord, DependencyManager}
import scaled.{AbstractService, Service}

/** A service for doing Maven stuffs. Is a service solely so we have a JVM-wide singleton. */
@Service(name="maven", impl="MavenService", desc="Handles Maven stuffs.")
class MavenService extends AbstractService {

  private val depmgr = new DependencyManager(Maven.m2repo, null, false, false)
  // TODO: override logging methods?

  /** Attempts to download the sources for `id`. This is performed synchronously, so this method
    * should only be called from a background thread. Blocking the editor UI while a Maven artifact
    * is being downloaded is not kosher.
    * @return the path to the downloaded source, or `None` if the sources could not be downloaded.
    */
  def fetchSources (id :Project.RepoId) :Option[Path] = {
    val coord = new Coord(id.groupId, id.artifactId, id.version, "jar")
    coord.classifier = "sources"
    Option(depmgr.resolveDependencies(Arrays.asList(coord)).get(coord))
  }

  // TODO: should we just make these non-abstract in AbstractService...
  override def didStartup () {}
  override def willShutdown () {}
}
