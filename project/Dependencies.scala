import sbt._

object Dependencies {

  val catsVersion       = "2.0.0"
  val spireVersion      = "0.16.0"
  val monocleVersion    = "1.5.0"

  val jettyVersion = "9.4.8.v20171121"
  val logbackVersion = "1.2.3"

  // Dependencies for JVM part of code
  val deps = Def.setting(Seq(
    "com.chuusai"   %% "shapeless" % "2.3.3",
    "com.lihaoyi"   %% "fansi" % "0.2.7",
    "org.typelevel" %% "cats-core" % catsVersion,
    "com.lihaoyi"   %% "sourcecode" % "0.1.7", // Scala-JVM
    "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"
  ))

}
