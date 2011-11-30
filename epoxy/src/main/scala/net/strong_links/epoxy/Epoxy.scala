package net.strong_links.epoxy

import net.strong_links.core._

import sbt._
import sbt.Keys._

object Epoxy {
  
  
  def zis: net.strong_links.core.Logger = new {
    def debug(msg: String): Unit = println(msg)
    def info(msg: String): Unit = println(msg)
    def warning(msg: String): Unit = println(msg)
    def error(msg: String): Unit =  println(msg)
  }

  
  val epoxyTemplateRoots = TaskKey[Seq[File]]("epoxy-template-root-directories")
    
  val watchedTemplates = TaskKey[Seq[File]]("epoxy-watched-templates")
    
  def init = {      
      
    val epoxyTask = (TaskKey[Seq[File]]("epoxy") in Compile) <<= (
        organization,
        thisProject,
        watchedTemplates,
        epoxyTemplateRoots,
        (sourceManaged in Compile)) map { (org, proj, t, templateDirs, outDir) =>
      
      println("epoxy on : \n" + t.mkString("\n"))
      println("outDir : " + outDir)
      println("org : " + org)
      println("id : " + proj.id)
      
      val packageName = org + "." + proj.id + ".templates"
      //(logger: Logger, inputDirectory: File, outputDirectory: File, rootPackage: Option[String])
      
      templateDirs.flatMap(td => SbtTemplateRunner(zis, td, outDir, Some(packageName)))
      t      
    }
      
    Seq(
      watchedTemplates  <<= (epoxyTemplateRoots).map(etr => 
        etr.flatMap(d => (PathFinder(d) ** "*").get)
      ),
      watchSources <++= (watchedTemplates in Compile).identity,
      epoxyTask,
      (sourceGenerators in Compile) <+= (
        organization,
        thisProject,
        watchedTemplates,
        epoxyTemplateRoots,
        (sourceManaged in Compile)) map { (org, proj, t, templateDirs, outDir) =>
        
        println("epoxy on : \n" + t.mkString("\n"))
        println("outDir : " + outDir)
        println("org : " + org)
        println("id : " + proj.id)
        
        val packageName = org + "." + proj.id + ".templates"
        //(logger: Logger, inputDirectory: File, outputDirectory: File, rootPackage: Option[String])
        
        val res = templateDirs.flatMap(td => SbtTemplateRunner(zis, td, outDir, Some(packageName)))

        println(res.mkString("\n"))
        
        res
      }
    )
  }
}
