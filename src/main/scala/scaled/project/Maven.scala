//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// http://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import java.io.File
import java.nio.file.{Path, Paths}
import pomutil.{DependResolver, Dependency, POM}

/** Some shared Maven-related utilities. */
object Maven {
  import Project._

  /** Converts a `Dependency` to a `RepoId`. */
  def toId (dep :Dependency) = RepoId(MavenRepo, dep.groupId, dep.artifactId, dep.version)

  /** Resolves the `.pom` file for `id`. */
  def resolvePOM (id :RepoId) = resolve(id, "pom", None)

  /** Resolves the binary `.jar` file for `id`. */
  def resolveClasses (id :RepoId) = resolve(id, "jar", None)

  /** Resolves the sources `.jar` file for `id`. */
  def resolveSources (id :RepoId) = resolve(id, "jar", Some("sources"))

  /** Resolves the `.m2` `.ext` file for `id`. */
  def resolve (id :RepoId, ext :String, classifier :Option[String] = None) :Path = {
    val csuff = classifier.map(c => s"-$c").getOrElse("")
    val artifactName = s"${id.artifactId}-${id.version}$csuff.$ext"
    m2repo.resolve(id.groupId.replace('.', File.separatorChar)).resolve(id.artifactId).resolve(
      id.version).resolve(artifactName)
  }

  private val m2repo = Paths.get(System.getProperty("user.home"), ".m2", "repository")
}
