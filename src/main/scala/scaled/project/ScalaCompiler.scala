//
// Scaled Maven Project plugin - a Scaled extension for handling Maven projects
// http://github.com/scaled/maven-project/blob/master/LICENSE

package scaled.project

import pomutil.Dependency
import reactual.Promise
import scala.collection.mutable.ArrayBuffer
import scaled._
import scaled.util.Expecter

class ScalaCompiler (proj :MavenProject, exec :Executor, log :Logger) extends Compiler {
  import Compiler._
  import ScalaCompiler._

  val scDepend = Dependency("com.samskivert.scaled", "scala-compiler", "1.0-SNAPSHOT")
  val scJar = scDepend.localArtifact getOrElse {
    throw new Exception("Unable to resolve $scDepend")
  }

  val expecter = Expecter.withLogger(exec, log, "maven-scalac", "java", "-jar", scJar.getPath)
  expecter.send(s"output ${proj.outputDir.getAbsolutePath}")
  expecter.send(s"classpath ${proj.buildClasspath.map(_.getAbsolutePath).mkString("\t")}")

  def compile (buffer :Buffer) = {
    val result = Promise[Boolean]()
    val cmds = Seq("compile") ++ proj.sourceDirs.map(_.getAbsolutePath)
    expecter.interact(Seq(cmds.mkString(" "))) { (line, isErr) =>
      line match {
        case "compile success" => result.succeed(true) ; true
        case "compile failure" => result.succeed(false) ; true
        case _ => buffer.append(Line.fromTextNL(line)) ; false
      }
    }
    result
  }

  def nextError (buffer :Buffer, start :Loc) = {
    buffer.findForward(pathM, start) match {
      case Loc.None => None
      case ploc => try {
        val file = pathM.group(1)
        val line = pathM.group(2).toInt
        val errPre = pathM.group(3).trim
        val pnext = ploc.nextStart
        // now search for the caret that indicates the error column
        buffer.findForward(caretM, pnext) match {
          case Loc.None => Some(Error(file, Loc(line-1, 0), errPre) -> pnext)
          case cloc =>
            val desc = Line.toText(Line(errPre) +: buffer.region(pnext, cloc.atCol(0)))
            Some(Error(file, Loc(line-1, cloc.col), desc) -> cloc.nextStart)
        }
      } catch {
        case e :Exception => log.log("Error parsing error buffer", e) ; None
      }
    }
  }

  def shutdown () {
    expecter.close()
  }
}

object ScalaCompiler {

  // matches: "/foo/bar/baz.scala:NN: some error message"
  val pathM = Matcher.regexp("""^(\S+):(\d+):(.*)""")
  // matches: "     ^"
  val caretM = Matcher.regexp("""^(\s*)\^""")
}
