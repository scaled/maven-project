//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// https://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import scaled._

import java.io.FileOutputStream
import java.nio.file.{Files, Path, Paths}
import java.util.jar.{JarEntry, JarFile}
import java.util.stream.Collectors
import pomutil.Dependency

object Android {

  def jarsForAar (dep :Dependency, exec :Executor) :SeqV[Path] = {
    val aarPath = Maven.resolve(dep)
    if (!Files.exists(aarPath)) {
      exec.handleError(new Exception(s"Missing $aarPath, cannot extract embedded jars."))
      return Seq()
    }
    val xdir = aarPath.getParent().resolve("extracted")
    def listJars = Seq.view(Files.list(xdir).collect(Collectors.toList[Path]))

    // if the 'extracted' directory exists and contains jars, then we're done
    val xdirExists = Files.exists(xdir)
    if (xdirExists) {
      val jars = listJars
      if (!jars.isEmpty) return jars
    }

    // we may or may not need to create the 'extracted' directory
    if (!xdirExists) Files.createDirectory(xdir);

    // now we can extract the jar files from this aar; such android, so design
    val aar = new JarFile(aarPath.toFile)
    val enumEntries = aar.entries()
    while (enumEntries.hasMoreElements) {
      val entry = enumEntries.nextElement.asInstanceOf[JarEntry]
      val entryName = entry.getName
      if (entryName.endsWith(".jar")) {
        // TODO: this is probably not a good idea, but I'm going to do it anyway in the name of
        // expedience...
        val entryFileName = entryName.lastIndexOf('/') match {
          case -1 => entryName
          case n  => entryName.substring(n+1)
        }
        val jarPath = xdir.resolve(entryFileName)
        val is = aar.getInputStream(entry)
        val fos = new FileOutputStream(jarPath.toFile)
        while (is.available() > 0) {  // write contents of 'is' to 'fos'
          fos.write(is.read())
        }
        fos.close()
        is.close()
      }
    }

    // finally enumerate the jars we just unpacked
    listJars
  }
}
