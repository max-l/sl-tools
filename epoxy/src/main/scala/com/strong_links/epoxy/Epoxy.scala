package com.strong_links.epoxy

import com.strong_links.core._
import sbt._
import sbt.Keys._
import sbt.ResolvedProject

object Epoxy {

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

  val epoxyTemplateRoots = TaskKey[Seq[File]]("epoxy-template-root-directories")

  val epoxyResourceRoots = TaskKey[Seq[File]]("epoxy-resource-root-directories")

  private val watchedTemplates = TaskKey[Seq[File]]("epoxy-watched-templates")

  private val watchedResources = TaskKey[Seq[File]]("epoxy-watched-resources")

  private def mkRootPackage(org: String, p: ResolvedProject, w: String) = {
    val x = org + "." + p.id + w
    Some(x.replace("-", "_"))
  }

  private def defineEpoxyTask(forCompile: Boolean) = (
    logLevel,
    streams,
    organization,
    thisProject,
    watchedTemplates,
    watchedResources,
    epoxyTemplateRoots,
    epoxyResourceRoots,
    (sourceManaged in Compile)) map { (lLevel, streams, org, proj, wt, wr, templateDirs, resourceDirs, outDir) =>

      Logging.using(wrapSbtLogger(streams.log, lLevel)) {
        val res1 = templateDirs.flatMap(td => if (td.exists) SbtTemplateRunner(td, outDir, mkRootPackage(org, proj, ".templates")) else Nil)
        val res2 = resourceDirs.flatMap(rd => if (rd.exists) SbtResourceRunner(rd, outDir, mkRootPackage(org, proj, ".resources")) else Nil)

        if (forCompile)
          res1 ++ res2
        else
          wt ++ wr
      }
    }

  def init = {

    val epoxyTask = (TaskKey[Seq[File]]("epoxy") in Compile) <<= defineEpoxyTask(false)

    Seq(
      (epoxyTemplateRoots) <<= sourceDirectory.map(src => Nil: Seq[File]),
      (epoxyResourceRoots) <<= sourceDirectory.map(src => Nil: Seq[File]),
      watchedTemplates <<= (epoxyTemplateRoots in Compile).map(etr =>
        etr.flatMap(d => (PathFinder(d) ** "*").get)),
      watchedResources <<= (epoxyResourceRoots in Compile).map(err =>
        err.flatMap(d => (PathFinder(d) ** "*").get)),
      watchSources <++= (watchedTemplates in Compile),
      watchSources <++= (watchedResources in Compile),
      epoxyTask,
      (sourceGenerators in Compile) <+= defineEpoxyTask(true))
  }
}
