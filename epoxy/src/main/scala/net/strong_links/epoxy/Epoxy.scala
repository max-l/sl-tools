package net.strong_links.epoxy

import net.strong_links.core._

import sbt._
import sbt.Keys._

object Epoxy {

  private def wrapSbtLogger(sbtLogger: sbt.Logger): net.strong_links.core.Logger = new {
    def debug(msg: String): Unit = sbtLogger.debug(msg)
    def info(msg: String): Unit = sbtLogger.info(msg)
    def warning(msg: String): Unit = sbtLogger.warn(msg)
    def error(msg: String): Unit = sbtLogger.error(msg)
  }

  val epoxyTemplateRoots = TaskKey[Seq[File]]("epoxy-template-root-directories")

  val epoxyResourceRoots = TaskKey[Seq[File]]("epoxy-resource-root-directories")

  private val watchedTemplates = TaskKey[Seq[File]]("epoxy-watched-templates")

  private val watchedResources = TaskKey[Seq[File]]("epoxy-watched-resources")

  private def defineEpoxyTask(forCompile: Boolean) = (
    streams,
    organization,
    thisProject,
    watchedTemplates,
    watchedResources,
    epoxyTemplateRoots,
    epoxyResourceRoots,
    (sourceManaged in Compile)) map { (streams, org, proj, wt, wr, templateDirs, resourceDirs, outDir) =>

      val res1 = templateDirs.flatMap(td => if (td.exists) SbtTemplateRunner(wrapSbtLogger(streams.log), td, outDir, org + "." + proj.id + ".templates") else Nil)
      val res2 = resourceDirs.flatMap(rd => if (rd.exists) SbtResourceRunner(wrapSbtLogger(streams.log), rd, outDir, org + "." + proj.id + ".resources") else Nil)

      if (forCompile)
        res1 ++ res2
      else
        wt ++ wr
    }

  def init = {

    val epoxyTask = (TaskKey[Seq[File]]("epoxy") in Compile) <<= defineEpoxyTask(false)
    val emptyFileSeqTask = (sourceDirectories) map (d => Nil: Seq[File])

    Seq(
      //(epoxyTemplateRoots) <++= emptyFileSeqTask,
      //(epoxyResourceRoots) <++= emptyFileSeqTask,      
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
