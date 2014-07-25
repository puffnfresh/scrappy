scalaVersion := "2.11.2"

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _)

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.4" % "test"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)

libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
  case Some((2, 10)) ⇒
    Seq("org.scalamacros" %% "quasiquotes" % "2.0.1")
  case _ ⇒
    Seq()
})

unmanagedSourceDirectories in Compile <++= (scalaBinaryVersion, baseDirectory) { (version, dir) =>
  Seq(dir / "src" / "main" / ("scala-" + version))
}
