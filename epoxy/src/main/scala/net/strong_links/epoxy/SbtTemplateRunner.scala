package net.strong_links.epoxy

import net.strong_links.core._

import java.io.File

object SbtTemplateRunner {
  def apply(inputDirectory: File, outputDirectory: File, rootPackage: Option[String]) = {
    new TemplateScanner().run(inputDirectory, outputDirectory, rootPackage, false)
  }
}