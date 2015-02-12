package scalan
package compilation
package lms
package scalac //Underscore is added intentionally to avoid name clash with root scala package

//import java.io._
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.StoreReporter
import scalan.util.{FileUtil, ProcessUtil, StringUtil}
import java.io.File.{separator => s}
import java.io._
import java.net.{URL, URLClassLoader}
import scalan.util.FileUtil.copyToDir
import scala.collection.mutable

trait LmsCompilerScala extends LmsCompiler { self: ScalanCtxExp =>

  /**
   * If scalaVersion is None, uses scala-compiler.jar
   *
   * Otherwise uses SBT to compile with the desired version
   */
  case class CompilerConfig(scalaVersion: Option[String], extraCompilerOptions: Seq[String])

  implicit val defaultCompilerConfig = CompilerConfig(None, Seq.empty)

  case class CustomCompilerOutput(jars: Array[URL])

  def graphPasses(compilerConfig: CompilerConfig) = Seq(AllUnpackEnabler, AllInvokeEnabler)

  val extensionsJars = mutable.HashSet.empty[String]
  var mainJars = Set.empty[String]
  val libs = "lib"

  protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, graphVizConfig: GraphVizConfig)
                                       (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]) = {
    /* LMS stuff */
    val outputSource = new File(sourcesDir, functionName + ".scala")
    val buildSbtFile = new File(sourcesDir, "build.sbt")

    (createManifest(eInput), createManifest(eOutput)) match {
      case (mA: Manifest[a], mB: Manifest[b]) =>
        val bridge = makeBridge[a, b]
        mainJars = bridge.methodReplaceConf.libPaths.map(j => s"${new File("").getAbsolutePath}$s$libs$s$j")
        val dir = new File(s"${new File("").getAbsolutePath}$s$libs$s").listFiles()
        dir match {
          case _: Array[File] => dir.filter(_.getName.toLowerCase.endsWith(".jar")).foreach(f => {
            mainJars = mainJars + f.getAbsolutePath
            copyToDir(f, new File(s"${executableDir.getAbsolutePath}$s$libs$s"))
          })
          case _ =>
        }

        val facade = bridge.getFacade(graph.asInstanceOf[bridge.scalan.PGraph])
        val codegen = bridge.lms.codegen

        FileUtil.withFile(outputSource) { writer =>
          codegen.emitSource[a, b](facade.apply, functionName, writer)(mA, mB)
          codegen.emitDataStructures(writer)
        }

        val jarFile = FileUtil.file(executableDir.getAbsoluteFile, s"$functionName.jar")
        val jarPath = jarFile.getAbsolutePath
        FileUtil.deleteIfExist(jarFile)

        val logFile=FileUtil.file(executableDir.getAbsoluteFile, s"$functionName.log")
        FileUtil.deleteIfExist(logFile)

        compilerConfig.scalaVersion match {
          case Some(scalaVersion) =>
            val buildSbtText =
              s"""name := "$functionName"
                 |
                 |scalaVersion := "$scalaVersion"
                 |
                 |artifactPath in Compile in packageBin := file("$jarPath")
                 |
                 |scalacOptions ++= Seq(${compilerConfig.extraCompilerOptions.map(StringUtil.quote).mkString(", ")})
                 |""".stripMargin

            FileUtil.write(buildSbtFile, buildSbtText)

            val command = Seq("sbt", "package")

            ProcessUtil.launch(sourcesDir, command: _*)
          case None =>
            val settings = new Settings
            settings.usejavacp.value = true
            // necessary to lauch compiler
            // see http://stackoverflow.com/questions/27934282/object-scala-in-compiler-mirror-not-found-running-scala-compiler-programatical
            settings.embeddedDefaults[LmsCompilerScala]
            val compilerOptions = "-d" :: jarPath :: compilerConfig.extraCompilerOptions.toList
            settings.processArguments(compilerOptions, false)
            val reporter = new StoreReporter
            val compiler: Global = new Global(settings, reporter)
            val run = new compiler.Run
            val res = run.compile(List(outputSource.getAbsolutePath))

            import java.io.PrintWriter
            val S = new PrintWriter(logFile)
            S.println(settings)
            S.println(s"${settings.classpath}\n")
            for(row <- reporter.infos) {
              val pos = s"${row.pos.source.path}:${row.pos.safeLine}"
              val line1 = s"${row.severity}: ${pos}"
              val line2 = "|"+row.pos.lineContent
              val line3 = "|"+" "*(row.pos.column-1) + "^"
              S.println(line1)
              S.println(line2)
              S.println(line3)
            }
            S.println(s"class $functionName compiled with ${reporter.WARNING.count} warnings")
            S.close()

            reporter.ERROR.count match {
              case 0 => {} //println(s"class $functionName compiled with ${reporter.WARNING.count} warnings")
              case _ => throw new Exception(s"class $functionName compiled with ${reporter.ERROR.count} errors and ${reporter.WARNING.count} warnings")
            }

            res
        }

        var urls = scala.Array(jarFile.toURI.toURL)
        new File(s"${executableDir.getAbsolutePath}$s$libs").listFiles() match {
          case ar: scala.Array[File] => urls = urls ++ ar.filter { case f => f.getName.toLowerCase.endsWith(".jar")}.map(_.toURI.toURL)
          case _ =>
        }
        CustomCompilerOutput(urls)
    }
  }

  def loadMethod(compilerOutput: CompilerOutput[_, _]) = {
    // ensure Scala library is available
    val classLoader = new URLClassLoader(compilerOutput.custom.jars, classOf[_ => _].getClassLoader)
    val cls = classLoader.loadClass(compilerOutput.common.name)
    val argumentClass = compilerOutput.common.eInput.classTag.runtimeClass
    (cls, cls.getMethod("apply", argumentClass))
  }

  protected def doExecute[A, B](compilerOutput: CompilerOutput[A, B], input: A): B = {
    val (cls, method) = loadMethod(compilerOutput)
    val instance = cls.newInstance()
    val result = method.invoke(instance, input.asInstanceOf[AnyRef])
    result.asInstanceOf[B]
  }
}
