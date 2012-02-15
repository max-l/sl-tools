package com.strong_links.i18ngen
import sbt._
import sbt.Keys._
import com.strong_links.core._

object I18nGen {

  private def wrapSbtLogger(sbtLogger: sbt.Logger, _logLevel: Level.Value): com.strong_links.core.Logging.GenericLogger = new {

    import Level._

    // Note the following values for the log levels.
    //   - error = 4
    //   - warn =  3
    //   - info =  2
    //   - debug = 1
    def isErrorEnabled(): Boolean = _logLevel <= Error
    def isWarnEnabled(): Boolean = _logLevel <= Warn
    def isInfoEnabled(): Boolean = _logLevel <= Info
    def isDebugEnabled(): Boolean = _logLevel <= Debug
    def debug(msg: String): Unit = sbtLogger.debug(msg)
    def info(msg: String): Unit = sbtLogger.info(msg)
    def warn(msg: String): Unit = sbtLogger.warn(msg)
    def error(msg: String): Unit = sbtLogger.error(msg)
  }

  val i18nConfigs = TaskKey[Seq[I18nConfig]]("i18ngen-config")

  val fuzzyMatchTreshold = TaskKey[Option[Double]]("i18ngen-fuzzy-threshold")

  val epoxyTemplateRoots = TaskKey[Seq[File]]("epoxy-template-root-directories")

  def i18nTaskInvoker(f: RunConfig => Seq[File]) = (
    sbt.Keys.logLevel,
    streams,
    thisProject,
    i18nConfigs,
    fuzzyMatchTreshold,
    epoxyTemplateRoots,
    unmanagedSourceDirectories in Compile,
    sourceManaged in Compile).map { (lLevel, streams, org, i18nConfigs, fuzz, templateDirs, srcDirs, outDir) =>

      def getSingleDirectory(files: Seq[File], dirName: String) = files.filter(_.getName.endsWith(dirName)) match {
        case Seq() => Errors.fatal("No directory ending in _ found in supplied list _." << (dirName, files))
        case Seq(single) => single
        case seq => Errors.fatal("More than one directory ending in _ found: _." << (dirName, seq))
      }

      val scalaSrcDir = getSingleDirectory(srcDirs, "scala")

      val conf = new RunConfig(i18nConfigs, fuzz, scalaSrcDir, outDir, outDir)

      if (scalaSrcDir == None)
        Nil
      else {
        Logging.setLogger(c => wrapSbtLogger(streams.log, lLevel))
        val r = f(conf)
        Logging.resetLogger
        r
      }
    }

  def init = {

    val showConfig = i18nTaskInvoker(I18nShowConfig.run)

    val i18nShowConfigTask = (TaskKey[Seq[File]](
      "i18n-show-config",
      "Show the current i18n configuration.") in Compile) <<= showConfig

    val testConfig = i18nTaskInvoker(I18nTestConfig.run)

    val i18nTestConfigTask = (TaskKey[Seq[File]](
      "i18n-test-config",
      "Test the current i18n configuration for input user locales.") in Compile) <<= testConfig

    val genCat = i18nTaskInvoker(I18nGenerateCatalogs.run)

    val i18nGenCatalogTask = (TaskKey[Seq[File]](
      "i18n-generate-catalogs",
      "Generate catalog definitions that will typically be referred to by the package.scala files.") in Compile) <<= genCat

    val scanAndMerge = i18nTaskInvoker(I18nScanAndMerge.run)

    val i18nScanAndMergeTask = (TaskKey[Seq[File]](
      "i18n-scan-and-merge",
      "Scan Scala source files for I18n strings and merge them into PO files.") in Compile) <<= scanAndMerge

    val genRes = i18nTaskInvoker(I18nGenerateResources.run)

    val i18nGenerateResourcesTask = (TaskKey[Seq[File]](
      "i18n-generate-resources",
      "Generate Scala I18n resource classes that will be loaded at run-time.") in Compile) <<= genRes

    Seq(
      fuzzyMatchTreshold := Some(RunConfig.DEFAULT_FUZZY_THRESHOLD),
      sourceGenerators in Compile <+= genCat,
      sourceGenerators in Compile <+= genRes,
      i18nShowConfigTask,
      i18nTestConfigTask,
      i18nGenCatalogTask,
      i18nScanAndMergeTask,
      i18nGenerateResourcesTask)
  }
}