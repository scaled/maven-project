//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// http://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import java.io.File
import scaled.Plugin

class MavenProject (root :File) extends FileProject(root, MavenProject.mavenIgnores) {

  // TODO: more stuffs!
}

object MavenProject {

  // TODO: don't do things this way, determine the classes directory from the POM, etc.
  val mavenIgnores = FileProject.fileIgnores ++ Set("target")

  @Plugin(tag="project-finder")
  class FinderPlugin extends ProjectFinderPlugin("maven", true) {
    // TODO: figure out how we'll make life easier in multi-module projects
    def checkRoot (root :File) :Int = if (new File(root, "pom.xml").exists()) 1 else -1
    def createProject (root :File) :Project = new MavenProject(root)
  }
}
