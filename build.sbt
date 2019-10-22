import Dependencies._

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

resolvers += Resolver.sonatypeRepo("releases")

Compile / compile := (Compile / compile).dependsOn(
  Def.task {
    streams.value.log.info("\n")
    streams.value.log.info("Maybe it will work this time...")
    streams.value.log.info("\n\n\n\n\n\n\n")
  }
).value

ThisBuild / version := "0.1.0"
inThisBuild(Seq(
  scalacOptions ++= compilerFlags
))

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")

lazy val root = (project in file("."))
  .settings(
    name := "scalapagos",
    scalaVersion := "2.13.0",
    libraryDependencies ++= deps.value
  )



val compilerFlags = Seq(
    // Emit warning and location for usages of features that should be imported explicitly.
    // Det trenger vi vel egentlig ikke, blir bare masse støy
    // "-feature",

    "-explaintypes",

    // Lol no HKT
    "-language:higherKinds",

    // Under tvil, kan fint taes av lokalt
    "-deprecation",

    // Enable additional warnings where generated code depends on assumptions.
    "-unchecked",

    // Allow definition of implicit functions called views
    "-language:implicitConversions",

    // Existential types (besides wildcard types) can be written and inferred
    "-language:existentials",


    // Allows 2 second as opposed to 2.second
    // Generally adviced against, but I kinda like having them
    "-language:postfixOps",

    // Makes inference work better for partially parametrized types
    // such as foo[F[_], A]
    // "-Ypartial-unification",

    // Completery moronic discarding of values.
    // Had a really nasty bug where IO[IO[Unit]] was accepted when the signature was IO[Unit]
    // Nightmare to debug...
    "-Ywarn-value-discard"


    // Yeah no
    // "-Xfatal-warnings",
    // "-Xlint:_,-missing-interpolator,-adapted-args"
)

cancelable in Global := true
fork := true
