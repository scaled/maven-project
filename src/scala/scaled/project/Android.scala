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

  def jarsForAar (dep :Dependency) :SeqV[Path] = {
    val aarPath = Maven.resolve(dep)
    val m2dir = aarPath.getParent()
    val xdir = m2dir.resolve("extracted")
    // if we haven't extracted the jar files from this aar yet, do so
    if (!Files.exists(xdir)) {
      Files.createDirectory(xdir);
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
    }
    // now list all the jars in xdir
    Seq.view(Files.list(xdir).collect(Collectors.toList[Path]))
  }
}
