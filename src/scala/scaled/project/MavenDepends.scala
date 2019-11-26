//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// http://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import java.io.File
import java.nio.file.Path
import pomutil.{DependResolver, Dependency, POM}
import scaled._
import scaled.pacman.JDK

/** Converts Maven depends to Scaled projects. */
class MavenDepends (project :Project, val pom :POM, isMain :Boolean) extends Depends(project) {
  import Project._

  override def ids = {
    val deps = Seq.builder[Id]
    if (isMain) deps ++= buildTransitive
    else {
      // if this is the test subproject, add a depend on the main project
      deps += RepoId(MavenRepo, pom.groupId, pom.artifactId, pom.version)
      deps ++= testTransitive
    }
    deps += platformDepend
    deps.build()
  }

  /** Returns the version of the specified artifact in our depends, or `defvers` if the specified
    * depend does not occur in our depends. */
  def artifactVers (groupId :String, artifactId :String, defvers :String) =
    (ids collectFirst {
      case RepoId(_, gid, aid, vers) if (gid == groupId && aid == artifactId) => vers
    }) getOrElse defvers

  /** Resolves build (main) depends for [[pom]], using `pspace` to resolve projects known to Scaled
    * in lieu of artifacts in the local Maven repo. */
  def buildTransitive :Seq[Project.Id] = transitiveDepends(DependResolver.Compile) flatMap(toId)

  /** Resolves test depends for [[pom]], using `pspace` to resolve projects known to Scaled in lieu
    * of artifacts in the local Maven repo. */
  def testTransitive :Seq[Project.Id] = transitiveDepends(DependResolver.Test) flatMap(toId)

  def buildClasspath = classpath(DependResolver.Compile)
  def testClasspath = classpath(DependResolver.Test)
  def execClasspath = classpath(DependResolver.Runtime)

  // TODO: infer the desired Java version from the maven-compiler-plugin POM section?
  def platformDepend = PlatformId(JavaPlatform, "8"/*JDK.thisJDK.majorVersion*/)

  // if a dependency has a classifier, don't turn it into a Scaled project
  private def toId (dep :Dependency) = if (dep.classifier.isDefined) None else Some(mkId(dep))
  private def mkId (dep :Dependency) = RepoId(MavenRepo, dep.groupId, dep.artifactId, dep.version)

  protected def classpath (scope :DependResolver.Scope) :Seq[Path] =
    transitiveDepends(scope) flatMap dependClasses

  private def transitiveDepends (scope :DependResolver.Scope) :Seq[Dependency] = {
    val dr = new DependResolver(pom) {
      // check whether this dependency is known to Scaled, and use the known version's POM instead
      // of the default POM (which comes from ~/.m2 and may be stale for snapshot depends)
      override def localDep (dep :Dependency) =
        toId(dep).flatMap(projectPOM).toScala orElse super.localDep(dep)
      private def projectPOM (id :RepoId) = pspace.knownProjectFor(id).map(_.depends) match {
        case Some(md :MavenDepends) => Some(md.pom)
        case _                      => None
      }
    }
    Seq(dr.resolve(scope) :_*)
  }

  private def dependClasses (dep :Dependency) :SeqV[Path] =
    javaCompClasses(dep) || (dep.`type` match {
      case "aar" => Android.jarsForAar(dep, pspace.wspace.exec)
      case _     => Seq(Maven.resolve(dep))
    })

  // if a project has a JavaComponent use its classes directories
  private def javaCompClasses (dep :Dependency) :Option[SeqV[Path]] = for {
    id <- toId(dep) ; proj <- pspace.knownProjectFor(id) ;
    java <- proj.component(classOf[JavaComponent])
  } yield java.classes

  private def pspace = project.pspace
}
