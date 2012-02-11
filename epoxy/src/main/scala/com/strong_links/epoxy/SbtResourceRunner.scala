package com.strong_links.epoxy

import com.strong_links.core._

import java.io.File

object SbtResourceRunner {
  def apply(inputDirectory: File, outputDirectory: File, rootPackage: Option[String], rebuild: Boolean) = {
    (new ResourceScanner).run(inputDirectory, outputDirectory, rootPackage, rebuild)
  }
}