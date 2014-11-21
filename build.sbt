organization := "me.gvdm"

name := "Basic Lift"

version := "0.1"

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

resolvers ++= Seq("snapshots"     at "https://oss.sonatype.org/content/repositories/snapshots",
                  "staging"       at "https://oss.sonatype.org/content/repositories/staging",
                  "releases"      at "https://oss.sonatype.org/content/repositories/releases"
                 )

libraryDependencies ++= {
  val liftVersion = "3.0-M2"
  val liftEdition = liftVersion.substring(0,3)
  Seq(
    "net.liftweb"		%% "lift-webkit"			% liftVersion		% "compile",
    "net.liftmodules"	        %% ("lift-jquery-module_"+liftEdition)  % "2.9-SNAPSHOT",
    "net.liftweb" %% "lift-mongodb-record" % liftVersion,
    "net.liftmodules" %% ("mongoauth_" + liftEdition) % "0.6-SNAPSHOT",
    "org.eclipse.jetty"         % "jetty-webapp"		     	% "8.1.13.v20130916" % "container,test",
    "org.eclipse.jetty.orbit"   % "javax.servlet"			% "3.0.0.v201112011016" % "container,compile" artifacts Artifact("javax.servlet", "jar", "jar")
  )
}

seq(webSettings :_*)

// Remove Java directories, otherwise sbteclipse generates them
unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_))

unmanagedSourceDirectories in Test <<= (scalaSource in Test)(Seq(_))

EclipseKeys.withSource := true

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

unmanagedResourceDirectories in Compile <+= (baseDirectory) { _ / "src/main/webapp" }

unmanagedResourceDirectories in Test <+= (baseDirectory) { _ / "src/test/webapp" }
