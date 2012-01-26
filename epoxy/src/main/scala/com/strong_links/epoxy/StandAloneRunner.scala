package com.strong_links.epoxy

import com.strong_links.core._

abstract class StandAloneRunner {

  import CmdLine._

  def inputDirectoryLabel: String
  def outputDirectoryLabel: String

  def main(args: Array[String])(scannerCreator: ScannerCreator) = {

    CmdLine(this, args).run(
      fileParameter("input directory", inputDirectoryLabel),
      fileParameter("output directory", outputDirectoryLabel),
      stringSwitch("root-package", "package name", "Root package name."),
      switch("rebuild", "Always rebuild all output files.")) {
        (inputDirectory, rootPackage, outputDirectory, rebuild) =>
          scannerCreator.run(inputDirectory, rootPackage, outputDirectory, rebuild)
      }
  }
}

