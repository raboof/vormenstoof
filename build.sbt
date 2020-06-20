val sharedSettings = Seq(
  organization := "net.bzzt",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test",
)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .settings(sharedSettings)
