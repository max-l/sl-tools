package com.strong_links.epoxy

import com.strong_links.core._

import java.io.File

object SbtTemplateRunner {
  def apply(inputDirectory: File, outputDirectory: File, rootPackage: Option[String], rebuild: Boolean) = {
    (new TemplateScanner).run(inputDirectory, outputDirectory, rootPackage, rebuild)
  }
}