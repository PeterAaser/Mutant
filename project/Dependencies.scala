import sbt._

object Dependencies {
  val versionOfScala = "2.12.9"

  val doobieVersion     = "0.6.0"
  val fs2Version        = "1.0.4"
  val http4sVersion     = "0.20.0-M3"  // 19 is EOL, 20 is milestone stage. YOLO
  val circeVersion      = "0.10.1"
  val catsVersion       = "1.5.0"
  val catsEffectVersion = "1.1.0"
  val spireVersion      = "0.16.0"
  val monocleVersion    = "1.5.0"

  val jettyVersion = "9.4.8.v20171121"
  val logbackVersion = "1.2.3"

  // Dependencies for JVM part of code
  val deps = Def.setting(Seq(
    "org.eclipse.jetty" % "jetty-server" % jettyVersion,
    "org.eclipse.jetty.websocket" % "websocket-server" % jettyVersion,

    // JSON
    "io.circe" %% "circe-core"     % circeVersion,
    "io.circe" %% "circe-generic"  % circeVersion,
    "io.circe" %% "circe-parser"   % circeVersion,
    "io.circe" %% "circe-literal"  % circeVersion,
    "org.http4s" %% "http4s-circe" % http4sVersion,

    // Abstract level category dork stuff
    "com.chuusai" %% "shapeless" % "2.3.3",

    // HTTP server and client
    "org.http4s" %% "http4s-dsl" % http4sVersion,
    "org.http4s" %% "http4s-blaze-server" % http4sVersion,
    "org.http4s" %% "http4s-blaze-client" % http4sVersion,
    "org.http4s" %% "http4s-server" % http4sVersion,
    "joda-time" % "joda-time" % "2.9.9",
    "org.joda" % "joda-convert" % "2.0.1",

    // IO and effects
    "org.typelevel" %% "cats-core" % catsVersion,
    "org.typelevel" %% "cats-effect" % catsEffectVersion,
    "org.typelevel" %% "cats-mtl-core" % "0.7.0",


    // 10/10
    "co.fs2" %% "fs2-core" % fs2Version,
    "co.fs2" %% "fs2-io"   % fs2Version,

    // Databases. Unironically uses comonads
    "org.tpolecat" %% "doobie-core"       % doobieVersion,
    "org.tpolecat" %% "doobie-postgres"   % doobieVersion,
    "org.tpolecat" %% "doobie-specs2"     % doobieVersion,

    "org.scalanlp" %% "breeze" % "0.13.2",
    
    // Native libraries are not included by default. add this if you want them (as of 0.7)
    // Native libraries greatly improve performance, but increase jar sizes. 
    // It also packages various blas implementations, which have licenses that may or may not
    // be compatible with the Apache License. No GPL code, as best I know.
    "org.scalanlp" %% "breeze-natives" % "0.13.2",
    
    // The visualization library is distributed separately as well.
    // It depends on LGPL code
    "org.scalanlp" %% "breeze-viz" % "0.13.2"
  ))

}
