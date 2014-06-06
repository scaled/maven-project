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

  /** A helper class for merging Maven depends with Scaled projects. */
  abstract class Depends (projectSvc :ProjectService) {

    /** Returns the POM we use to resolve depends. */
    def pom :POM

    /** Resolves depends for [[pom]], using `projectSvc` to resolve projects known to Scaled in lieu
      * of artifacts in the local Maven repo. */
    def transitiveDepends (forTest :Boolean) :Seq[Dependency] = new DependResolver(pom) {
      // check whether this dependency is known to Scaled, and use the known version's POM instead of
      // the default POM (which comes from ~/.m2 and may be stale for snapshot depends)
      override def localDep (dep :Dependency) = projectPOM(toId(dep)) orElse super.localDep(dep)
      private def projectPOM (id :RepoId) = projectSvc.projectFor(id) match {
        case Some(proj :MavenProject) => Some(proj.pom)
        case _ => None
      }
    }.resolve(forTest)

    /** Returns a classpath based on the dependencies in [[pom]] (using [[transitiveDepends]]).
      * @param forTest if true then the test dependencies will be included. */
    def classpath (forTest :Boolean) :Seq[Path] = transitiveDepends(forTest) map(toId) map { id =>
      projectSvc.projectFor(id) match {
        case Some(proj :JavaProject) => proj.classes
        case _                       => resolveClasses(id)
      }
    }
  }

  private val m2repo = Paths.get(System.getProperty("user.home"), ".m2", "repository")
}
