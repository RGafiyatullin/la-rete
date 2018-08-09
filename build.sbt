
lazy val root = (project in file("."))
  .settings(
    name := "la-rete",
    organization := "com.github.rgafiyatullin",
    version := BuildEnv.version,

    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
    scalacOptions ++= Seq("-language:implicitConversions"),
    scalacOptions ++= Seq("-Ywarn-value-discard", "-Xfatal-warnings"),

    scalaVersion := BuildEnv.scalaVersion,

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % {
        scalaVersion.value match {
          case v2_12 if v2_12.startsWith("2.12.") => "3.0.4"
          case v2_11 if v2_11.startsWith("2.11.") => "2.2.6"
        }
      },
      "com.github.rgafiyatullin" %% "xmpp-protocol" % "0.5.7.0"
    ),

    publishTo := BuildEnv.publishTo,
    credentials ++= BuildEnv.credentials.toSeq
  )

