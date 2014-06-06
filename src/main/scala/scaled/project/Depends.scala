//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// http://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import java.io.File
import java.nio.file.{Path, Paths}
import pomutil.{DependResolver, Dependency, POM}

/** A helper class for merging Maven depends with Scaled projects. */
abstract class Depends (projectSvc :ProjectService) {
  import Project._

  /** Returns the POM we use to resolve depends. */
  def pom :POM

  /** Resolves depends for [[pom]], using `projectSvc` to resolve projects known to Scaled in lieu
    * of artifacts in the local Maven repo. */
  def transitive (forTest :Boolean) :Seq[Project.Id] = transitiveDepends(forTest) map(toId)

  /** Returns a classpath based on the dependencies in [[pom]] (using [[transitiveDepends]]).
    * @param forTest if true then the test dependencies will be included. */
  def classpath (forTest :Boolean) :Seq[Path] =
    for (dep <- transitiveDepends(forTest) ; id = toId(dep))
    yield projectSvc.projectFor(id) match {
      case Some(proj :JavaProject) => proj.classes
      case _                       => dep.systemPath match {
        case Some(path) => Paths.get(path)
        case _          => Maven.resolveClasses(id)
      }
    }

  private def toId (dep :Dependency) = RepoId(MavenRepo, dep.groupId, dep.artifactId, dep.version)

  private def transitiveDepends (forTest :Boolean) = new DependResolver(pom) {
    // check whether this dependency is known to Scaled, and use the known version's POM instead of
    // the default POM (which comes from ~/.m2 and may be stale for snapshot depends)
    override def localDep (dep :Dependency) = projectPOM(toId(dep)) orElse super.localDep(dep)
    private def projectPOM (id :RepoId) = projectSvc.projectFor(id) match {
      case Some(proj :MavenProject) => Some(proj.pom)
      case _ => None
    }
  }.resolve(forTest)
}
