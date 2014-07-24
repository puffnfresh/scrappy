scalaVersion := "2.11.0-SNAPSHOT"

scalaOrganization := "org.scala-lang.macro-paradise"

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

libraryDependencies <+= (scalaVersion)("org.scala-lang.macro-paradise" % "scala-reflect" % _)

libraryDependencies += "org.scalacheck" % "scalacheck_2.10" % "1.11.4" % "test"
