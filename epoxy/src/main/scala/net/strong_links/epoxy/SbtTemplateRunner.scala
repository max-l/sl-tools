package net.strong_links.epoxy

import net.strong_links.core._

import java.io.File

object SbtTemplateRunner {
  def apply(loggers: Loggers, inputDirectory: File, outputDirectory: File, rootPackage: Option[String]) = {
    new TemplateScanner(loggers).run(inputDirectory, outputDirectory, rootPackage, false)
  }
}