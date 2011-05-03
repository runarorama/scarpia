import sbt._

class ScarpiaProject(info: ProjectInfo) extends DefaultProject(info)
{
  val scalazCore = "org.scalaz" %% "scalaz-core" % "6.0-SNAPSHOT"
}
