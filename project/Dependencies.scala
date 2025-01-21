import sbt.*
import sbt.librarymanagement.ModuleID
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*

/** V - Dependency Versions object */

object V {
  val sconfig = "1.8.1"
  val scopt = "4.1.0"
  val xml = "2.3.0"
}

object Dep {
  val sconfig = "org.ekrich" %% "sconfig" % V.sconfig
  val scopt = "com.github.scopt" %% "scopt" % V.scopt
  val xml = "org.scala-lang.modules" %% "scala-xml" % V.xml

}
