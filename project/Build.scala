
import sbt._
import Keys._


object Buildz extends Build {

  val core = "net.strong_links" %% "core" % "0.2"  
				   
  def buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "net.strong_links",
    version := "0.2",
    scalaVersion := "2.9.1",
    sbtPlugin := true,
	libraryDependencies ++= Seq(core)
  )      
	
  lazy val root = Project(
    id = "root",
    base = file("."),
	settings = buildSettings
  ) aggregate(templcomp, i18ngen)
  
  
  lazy val templcomp = Project(
    id = "templcomp",
    base = file("templcomp"),    
    settings = buildSettings
  )
  
  lazy val i18ngen = Project(
    id = "i18ngen",
    base = file("i18ngen"),    
    settings = buildSettings
  )
}