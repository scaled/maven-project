//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// http://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import java.io.File
import java.nio.file.{Path, Paths}
import pomutil.{DependResolver, Dependency, POM}

/** A helper class for merging Maven depends with Scaled projects. */
abstract class Depends (pspace :ProjectSpace) {
  import Project._

  /** Returns the POM we use to resolve depends. */
  def pom :POM

  /** Resolves depends for [[pom]], using `pspace` to resolve projects known to Scaled in lieu
    * of artifacts in the local Maven repo. */
  def transitive :Seq[Project.Id] = transitiveDepends(DependResolver.Compile) flatMap(toId)

  def buildClasspath = classpath(DependResolver.Compile)
  def testClasspath = classpath(DependResolver.Test)
  def execClasspath = classpath(DependResolver.Runtime)

  // TODO: infer the desired Java version from the maven-compiler-plugin POM section?
  def platformDepend = PlatformId(JavaPlatform, JDK.thisJDK.majorVersion)

  // if a dependency has a classifier, don't turn it into a Scaled project
  private def toId (dep :Dependency) = dep.classifier match {
    case None => Some(mkId(dep))
    case _    => None
  }
  private def mkId (dep :Dependency) = RepoId(MavenRepo, dep.groupId, dep.artifactId, dep.version)

  protected def classpath (scope :DependResolver.Scope) :Seq[Path] =
    transitiveDepends(scope) map { dep => toId(dep) match {
      case Some(id) => pspace.projectFor(id) match {
        case Some(proj :JavaProject) => proj.classes
        case _                       => dep.systemPath match {
          case Some(path) => Paths.get(path)
          case _          => Maven.resolveClasses(id)
        }
      }
      case None => Maven.resolve(mkId(dep), "jar", dep.classifier)
    }}

  private def transitiveDepends (scope :DependResolver.Scope) = new DependResolver(pom) {
    // check whether this dependency is known to Scaled, and use the known version's POM instead of
    // the default POM (which comes from ~/.m2 and may be stale for snapshot depends)
    override def localDep (dep :Dependency) =
      toId(dep).flatMap(projectPOM) orElse super.localDep(dep)
    private def projectPOM (id :RepoId) = pspace.projectFor(id) match {
      case Some(proj :MavenProject) => Some(proj.pom)
      case _                        => None
    }
  }.resolve(scope)
}
