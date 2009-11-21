import sbt._

class ScoogleProject(p : ProjectInfo) extends DefaultProject(p) {
  val specs = "org.scala-tools.testing" % "specs" % "1.6.0"
  val scalatest = "org.scala-tools.testing" % "scalatest" % "0.9.5"
}