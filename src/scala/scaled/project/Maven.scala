//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.io.File
import java.nio.file.{Path, Paths}
import pomutil.Dependency

/** Some shared Maven-related utilities. */
object Maven {
  import Project._

  /** The user's local Maven repository. */
  val m2repo = Paths.get(System.getProperty("user.home"), ".m2", "repository")

  /** Resolves the `.pom` file for `id`. */
  def resolvePOM (id :RepoId) = resolve(id, "pom")

  /** Resolves the sources `.jar` file for `id`. */
  def resolveSources (id :RepoId) = resolve(id, "jar", Some("sources"))

  /** Resolves the local artifact for `dep`. */
  def resolve (dep :Dependency) :Path =
    resolve(dep.groupId, dep.artifactId, dep.version, dep.`type`, dep.classifier)

  /** Resolves the `.m2` `.ext` file for `id`. */
  def resolve (id :RepoId, ext :String, classifier :Option[String] = None) :Path =
    resolve(id.groupId, id.artifactId, id.version, ext, classifier)

  /** Resolves the `.m2` `.ext` file for `id`. */
  def resolve (groupId :String, artifactId :String, version :String,
               ext :String, classifier :Option[String]) :Path = {
    val csuff = classifier.map(c => s"-$c").getOrElse("")
    val artifactName = s"$artifactId-$version$csuff.$ext"
    m2repo.resolve(groupId.replace('.', File.separatorChar)).resolve(artifactId).
      resolve(version).resolve(artifactName)
  }
}
