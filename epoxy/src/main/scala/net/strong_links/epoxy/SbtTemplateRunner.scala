package net.strong_links.epoxy

import net.strong_links.core._

import java.io.File

object SbtTemplateRunner {
  def apply(logger: Logger, inputDirectory: File, outputDirectory: File, rootPackage: String) = {
    new TemplateScanner(new Xlogger(logger)).run(inputDirectory, outputDirectory, rootPackage, false)
  }
}