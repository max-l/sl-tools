package com.strong_links.epoxy

import com.strong_links.core._
import com.strong_links.core.codegen._

import java.io.File

abstract class EpoxyScanner extends CodeGeneration with Logging {

  var hasError = false

  def getFileNameWithoutExtension(file: File) = {
    Errors.trap(file) {
      val segments = Util.split(file.getName, '.').filter(!_.isEmpty)
      if (segments.length < 2)
        Errors.fatal("Invalid file name")
      segments.dropRight(1).mkString
    }
  }

  def process(file: File, rootDirectory: File, outputDirectory: File, rootPackage: Option[String],
              rebuild: Boolean, i18nConfigs: Seq[I18nConfig]): Option[File]

  def scanFunction(file: File)(code: File => Unit)

  def run(inputDirectory: File, outputDirectory: File, rootPackage: Option[String], rebuild: Boolean, i18nConfigs: Seq[I18nConfig]) = {

    IO.checkForExistingDirectory(inputDirectory)
    IO.createDirectory(outputDirectory, true)

    var filesCreated: List[File] = Nil

    scanFunction(inputDirectory) {
      process(_, inputDirectory, outputDirectory, rootPackage, rebuild, i18nConfigs) match {
        case Some(f) => filesCreated = f :: filesCreated
        case None    =>
      }
    }

    if (hasError)
      Errors.fatal("Errors detected in source files.")

    logDebug("_ files provided to SBT: _" << (filesCreated.length, filesCreated))

    filesCreated.toList
  }

  import CmdLine._

  def main(args: Array[String], inputDirectoryLabel: String) {

    CmdLine(this, args).run(
      fileParameter("input directory", inputDirectoryLabel),
      fileParameter("output directory", "Generated code root directory."),
      stringParameter("specifications", "Specifications of packages and localizations."),
      stringSwitch("root-package", "root package name", "Root package name."),
      switch("rebuild", "Always rebuild all output files.")) {
        (inputDirectory, rootPackage, specifications, outputDirectory, rebuild) =>
          run(inputDirectory, rootPackage, outputDirectory, rebuild, I18nConfig.toI18nConfigs(specifications))
      }
  }
}
