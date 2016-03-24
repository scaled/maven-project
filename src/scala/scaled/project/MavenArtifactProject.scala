//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// http://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import codex.extract.{SourceSet, ZipUtils}
import codex.model.Source
import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.util.zip.ZipFile
import pomutil.POM
import scaled._

class MavenArtifactProject (af :MavenArtifactProject.Artifact, ps :ProjectSpace)
    extends AbstractZipFileProject(ps) with JavaProject {

  private def id = af.repoId
  private val _pom = POM.fromFile(af.pom.toFile) getOrElse {
    throw new IllegalArgumentException(s"Unable to load ${af.pom}")
  }
  private val _depends = new Depends(pspace) {
    def pom = _pom
  }

  override val root = Project.Root(af.sources, false)
  override val zipPaths = Seq(af.sources)
  override def isIncidental = true
  override def name = s"${id.artifactId}:${id.version}"
  override def idName = s"mvn-${id.groupId}_${id.artifactId}_${id.version}"
  override def ids = Seq(id)
  override def classes = {
    val dep = _pom.toDependency()
    _pom.packaging match {
      case "aar" => Android.jarsForAar(dep).head // TODO: return all
      case "jar" => Maven.resolve(dep)
      // this is a hack, but some projects publish a POM with pom packaging even though they ship
      // jars, or bundle packaging, or god knows... I guess maybe we're not supposed to look at the
      // packaging, but rather the 'type' field of the depender, so that will require some
      // revamping... sigh
      case _     => Maven.resolve(dep.copy(`type`="jar"))
    }
  }
  override def depends = _depends.buildTransitive :+ _depends.platformDepend
  override def buildClasspath :Seq[Path] = _depends.buildClasspath
  override def execClasspath :Seq[Path] = classes +: _depends.execClasspath

  override def summarizeSources = {
    // if our sources don't exist, try to download them
    val rpath = root.path
    if (!Files.exists(rpath)) {
      pspace.wspace.statusMsg.emit(s"Attempting to fetch sources: $rpath...")
      pspace.msvc.service[MavenService].fetchSources(af.repoId)
    }

    val mb = Map.builder[String,SourceSet]()
    if (Files.exists(rpath)) {
      val sources = new ZipFile(rpath.toFile)
      ZipUtils.summarizeSources(sources) foreach { suff =>
        mb += (suff, new SourceSet.Archive(rpath, ZipUtils.ofSuff("."+suff)))
      }
    }
    mb.build()
  }
}

object MavenArtifactProject {
  import Project._
  import Maven._

  case class Artifact (repoId :RepoId) {
    lazy val pom     = resolvePOM(repoId)
    lazy val sources = resolveSources(repoId)
  }

  @Plugin(tag="project-finder")
  class FinderPlugin extends ProjectFinderPlugin("mavenrepo", true, classOf[MavenArtifactProject]) {
    private def isJar (path :Path) = path.getFileName.toString endsWith ".jar"
    private def isInM2Repo (path :Path) = path startsWith Maven.m2repo
    override def checkRoot (root :Path) :Int = if (isJar(root) && isInM2Repo(root)) 1 else -1

    override def apply (id :Project.Id) = id match {
      case repoId @ RepoId(MavenRepo, _, _, _) =>
        val af = Artifact(repoId)
        if (Files.exists(af.pom)) Some(seed(Root(af.sources, false), af :: Nil))
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
    override protected def injectArgs (root :Root) = rootToArtifact(root.path) :: Nil
  }
}
