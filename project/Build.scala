
import sbt._
import Keys._


object Buildz extends Build {

  val core = "com.strong-links" %% "core" % "0.2"  

  def buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "com.strong-links",
    version := "0.2",
    scalaVersion := "2.9.1",
    sbtPlugin := true,
    libraryDependencies ++= Seq(core)
  )
  
  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = buildSettings
  ) aggregate(epoxy, i18ngen)
  
  
  lazy val epoxy = Project(
    id = "epoxy",
    base = file("epoxy"),    
    settings = buildSettings
  )
  
  lazy val i18ngen = Project(
    id = "i18ngen",
    base = file("i18ngen"),    
    settings = buildSettings
  )
}
