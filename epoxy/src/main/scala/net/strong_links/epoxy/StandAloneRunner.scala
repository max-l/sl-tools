package net.strong_links.epoxy

import net.strong_links.core._

abstract class StandAloneRunner {

  import CmdLine._

  def inputDirectoryLabel: String
  def outputDirectoryLabel: String

  def main(args: Array[String])(scannerCreator: ScannerCreator) = {

    CmdLine(this, args).run(
      fileParameter("input directory", inputDirectoryLabel),
      stringParameter("root package", "Root package."),
      fileParameter("output directory", outputDirectoryLabel),
      switch("rebuild", "Always rebuild all output files.")) {
        (logger, inputDirectory, rootPackage, outputDirectory, rebuild) =>
          scannerCreator(logger).run(inputDirectory, outputDirectory, rootPackage, rebuild)
      }
  }
}

