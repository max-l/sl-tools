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

  def i18nTaskInvoker(f: RunConfig => Seq[File]) = (
      sbt.Keys.logLevel,
      streams,
      thisProject,
      i18nConfigs,
      fuzzyMatchTreshold,
      unmanagedSourceDirectories in Compile,
      sourceManaged in Compile).map { (lLevel, streams, org, i18nConfigs, fuzz, srcDirs, outDir) =>

      val scalaSrcDir = srcDirs.filter(_.getName.endsWith("scala")).headOption
      val conf = new RunConfig(i18nConfigs, fuzz, scalaSrcDir.head, outDir)

      if(scalaSrcDir == None)
        Nil 
      else {
        Logging.logger = wrapSbtLogger(streams.log, lLevel)
        f(conf)         
      }
    }

  def init = {

    val genCat = i18nTaskInvoker(I18nGenerateCatalog.run)

    val i18nGenCatalogTask = (TaskKey[Seq[File]](
        "i18n-generate-catalogs", 
        "Generate catalog definitions that will typically be referred to by the package.scala files.") in Compile) <<= genCat

    val mergeAndScan = i18nTaskInvoker(I18nMerge.run)

    val i18nScanAndMergeTask = (TaskKey[Seq[File]](
        "i18n-scan-and-merge", 
        "Scan Scala source files for I18n strings and merge them into PO files.") in Compile) <<= mergeAndScan

    val genRes = i18nTaskInvoker(I18nGenerateResources.run)

    val i18nGenerateResourcesTask = (TaskKey[Seq[File]](
        "i18n-generate-resources", 
        "Generate Scala I18n resource classes that will be loaded at run-time.") in Compile) <<= genRes

    Seq(
      fuzzyMatchTreshold := Some(RunConfig.DEFAULT_FUZZY_THRESHOLD),
      sourceGenerators in Compile <+= genCat,
      sourceGenerators in Compile <+= genRes,
      i18nGenCatalogTask,
      i18nScanAndMergeTask,
      i18nGenerateResourcesTask
    )
  }
}