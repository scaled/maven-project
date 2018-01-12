//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// http://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import codex.extract.{SourceSet, ZipUtils}
import java.io.File
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile
import pomutil.POM
import scaled._

object MavenArtifactPlugins {
  import Project._
  import Maven._

  @Plugin(tag="project-root")
  class MavenArtifactRootPlugin extends RootPlugin {
    override def checkRoot (root :Path) :Int = if (isArtifact(root)) 1 else -1
    override def apply (id :Project.Id) = id match {
      case repoId @ RepoId(MavenRepo, _, _, _) =>
        val af = Artifact(repoId)
        if (Files.exists(af.pom)) Some(Root(af.sources, "")) else None
      case _ => None
    }
  }

  @Plugin(tag="project-resolver")
  class MavenArtifactResolverPlugin extends ResolverPlugin {
    override def addComponents (project :Project) {
      val root = project.root.path
      if (isArtifact(root)) {
        val af = rootToArtifact(root)
        project.pspace.wspace.exec.runAsync(loadPOM(af.pom)).
          onSuccess(addMavenComponents(project, af, _)).
          onFailure(project.pspace.wspace.exec.handleError)
      }
    }
  }

  private def addMavenComponents (project :Project, af :Artifact, pom :POM) {
    val id = af.repoId

    // add a filer component for our zip file(s)
    project.addComponent(classOf[Filer], new ZipFiler(Seq(af.sources)))

    // add a sources component that groks our zip-based source files
    project.addComponent(classOf[Sources], new Sources(Seq(af.sources)) {
      override def summarize = {
        // if our sources don't exist, try to download them
        val rpath = project.root.path
        if (!Files.exists(rpath)) {
          project.pspace.wspace.statusMsg.emit(s"Attempting to fetch sources: $rpath...")
          project.pspace.msvc.service[MavenService].fetchSources(af.repoId)
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
    })

    // add our dependency management component
    val depends = new MavenDepends(project, pom, true)
    project.addComponent(classOf[Depends], depends)

    val java = new JavaComponent(project)
    project.addComponent(classOf[JavaComponent], java)

    // init our Java component
    val classes = {
      val dep = pom.toDependency()
      pom.packaging match {
        case "aar" => Android.jarsForAar(dep, project.pspace.wspace.exec)
        case "jar" => Seq(Maven.resolve(dep))
        // this is a hack, but some projects publish a POM with pom packaging even though they
        // ship jars, or bundle packaging, or god knows... I guess maybe we're not supposed to
        // look at the packaging, but rather the 'type' field of the depender, so that will
        // require some revamping... sigh
        case _     => Seq(Maven.resolve(dep.copy(`type`="jar")))
      }
    }
    java.javaMetaV() = new JavaMeta(
      classes,
      java.javaMetaV().targetDir,
      java.javaMetaV().outputDir,
      depends.buildClasspath,
      classes ++ depends.execClasspath
    )

    // update our meta last so everything is ready for listeners who trigger on meta updates
    project.metaV() = Meta(s"${id.artifactId}:${id.version}", Set(id), None)
  }

  private def isArtifact (root :Path) = {
    def isJar (path :Path) = path.getFileName.toString endsWith ".jar"
    def isInM2Repo (path :Path) = path startsWith Maven.m2repo
    isJar(root) && isInM2Repo(root)
  }

  private def rootToArtifact (root :Path) :Artifact = {
    val versDir = root.getParent ; val version = versDir.getFileName.toString
    val artiDir = versDir.getParent ; val artifactId = artiDir.getFileName.toString
    val groupPath = Maven.m2repo.relativize(artiDir.getParent)
    val groupId = groupPath.toString.replace(File.separatorChar, '.')
    Artifact(RepoId(MavenRepo, groupId, artifactId, version))
  }
}
