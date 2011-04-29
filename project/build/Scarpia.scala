import sbt._

class ScarpiaProject(info: ProjectInfo) extends DefaultProject(info)
{
    lazy val scalaz = "com.googlecode.scalaz" % "scalaz-core_2.8.1" % "6.0-SNAPSHOT"
}
