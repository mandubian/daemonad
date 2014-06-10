scalaVersion := "2.11.0-RC1"

// Uncomment to test with a locally built copy of Scala.
// scalaHome := Some(file("/code/scala2/build/pack"))
resolvers ++= (if (scalaVersion.value.endsWith("SNAPSHOT")) List(Resolver.sonatypeRepo("snapshots")) else Nil)

organization := "com.mandubian"

name := "daemonad"

version := "0.1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scalaz"      %% "scalaz-core"            % "7.1.0-M6"                      ,
  "org.scala-lang"  %  "scala-reflect"          % scalaVersion.value  % "provided",
  "org.scala-lang"  %  "scala-compiler"         % scalaVersion.value  % "test"    ,
  "org.scalatest"   %  "scalatest_2.11"         % "2.1.3"             % "test"
)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s")

parallelExecution in Global := false

scalacOptions in compile ++= Seq("-optimize", "-deprecation", "-unchecked", "-Xlint", "-feature")

scalacOptions in Test ++= Seq("-Yrangepos")

scalacOptions in (Test, run) ++= Seq("-Yrangepos", "-Ddaemonad.debug=true")

javaOptions in console += "-Ddaemonad.debug=false"

// Generate $name.properties to store our version as well as the scala version used to build
resourceGenerators in Compile <+= Def.task {
  val props = new java.util.Properties
  props.put("version.number", version.value)
  props.put("scala.version.number", scalaVersion.value)
  props.put("scala.binary.version.number", scalaBinaryVersion.value)
  val file = (resourceManaged in Compile).value / s"${name.value}.properties"
  IO.write(props, null, file)
  Seq(file)
}

mappings in (Compile, packageBin) += {
   (baseDirectory.value / s"${name.value}.properties") -> s"${name.value}.properties"
}


description := "A categorical programming facility for Scala that offers a direct API for working with monad & a few monad stacks (at least trying)."

homepage := Some(url("http://github.com/mandubian/daemonad"))

startYear := Some(2014)

licenses +=("Apache2-license", url("http://www.apache.org/licenses/LICENSE-2.0.html"))

// Uncomment to disable test compilation.
// (sources in Test) ~= ((xs: Seq[File]) => xs.filter(f => Seq("TreeInterrogation", "package").exists(f.name.contains)))

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <developers>
    <developer>
      <id>mandubian</id>
      <name>Pascal Voitot</name>
      <timezone>+1</timezone>
      <url>http://github.com/mandubian</url>
    </developer>
  </developers>
    <scm>
      <url>git@github.com:mandubian/daemonad.git/</url>
      <connection>scm:git:git@github.com:mandubian/daemonad.git</connection>
    </scm>
  )