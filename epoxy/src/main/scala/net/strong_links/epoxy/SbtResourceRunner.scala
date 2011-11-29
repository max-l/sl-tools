package net.strong_links.epoxy

import net.strong_links.core._

import java.io.File

object SbtResourceRunner {
  def apply(logger: Logger, inputDirectory: File, outputDirectory: File, rootPackage: Option[String]) = {
    new ResourceScanner(logger).run(inputDirectory, outputDirectory, rootPackage, false)
  }
}