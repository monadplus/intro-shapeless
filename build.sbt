resolvers in Global += Resolver.sonatypeRepo("releases") // "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

lazy val catsVersion = "1.5.0"
lazy val shapelessVersion = "2.3.3"
lazy val kindProjectorVersion = "0.9.8"

lazy val commonDependencies = Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "com.chuusai" %% "shapeless" % shapelessVersion
)

lazy val compilerFlags = Seq(
  scalacOptions ++= Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding",
    "utf-8",
    "-explaintypes", // Explain type errors in more detail.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
    "-language:higherKinds", // Allow higher-kinded types
    "-language:implicitConversions", // Allow definition of implicit functions called views
    "-unchecked", // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
//    "-Xfatal-warnings", // Fail the compilation if there are any warnings.
    "-Xfuture", // Turn on future language features.
    "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
    "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
    "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
    "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
    "-Xlint:option-implicit", // Option.apply used implicit view.
    "-Xlint:package-object-classes", // Class or object defined in package object.
    "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match", // Pattern match may not be typesafe.
    "-Yno-adapted-args",    // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
//     "-Yno-imports",                      // No predef or default imports
    "-Ypartial-unification", // Enable partial unification in type constructor inference
    "-Ywarn-dead-code", // Warn when dead code is identified.
    "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
    "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen", // Warn when numerics are widened.
    "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals", // Warn if a local definition is unused.
    "-Ywarn-unused:params", // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates", // Warn if a private member is unused.
    "-Ywarn-value-discard" // Warn when non-Unit expression results are unused.
  ),
  scalacOptions in (Compile, console) ~= {
    _.filterNot("-Ywarn-unused:imports" == _)
      .filterNot("-Xlint" == _)
      .filterNot("-Xfatal-warnings" == _)
  },
  scalacOptions in (Test, compile) --= Seq(
    "-Ywarn-unused:privates",
    "-Ywarn-unused:locals",
    "-Ywarn-unused-import",
    "-Ywarn-unused:imports",
    "-Yno-imports"
  )
)

lazy val commonSettings: Seq[Def.Setting[_]] = Seq(
  organization := "org.statictypeddog",
  name := "intro-shapeless",
  version := "0.1",
  scalaVersion := "2.12.7",
  licenses ++= Seq(("MIT", url("http://opensource.org/licenses/MIT"))),
  parallelExecution in Test := true,
  fork in Test := true,
  libraryDependencies ++= commonDependencies,
//  scalafmtOnCompile := true, //Travis don't like it
  addCompilerPlugin("org.spire-math" %% "kind-projector" % kindProjectorVersion)
) ++ compilerFlags

lazy val root = project
  .in(file("."))
  .settings(commonSettings: _*)
  .settings(
    description := "Introduction to Shapeless",
    initialCommands in console := s"""
        import cats._, cats.data._, cats.implicits._
        import shapeless._
      """
  )