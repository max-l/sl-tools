package net.strong_links.epoxy

import net.strong_links.core._

import java.io.File

abstract class EpoxyScanner extends CodeGeneration with Logging {

  var hasError = false

  def getFileNameWithoutExtension(file: File) = {
    Errors.trap("File name _" << file) {
      val segments = Util.split(file.getName, '.').filter(!_.isEmpty)
      if (segments.length < 2)
        Errors.fatal("Invalid file name _." << file)
      Lex.normalizeName(segments.dropRight(1).mkString)
    }
  }

  def process(file: File, rootDirectory: File, outputDirectory: File, rootPackage: Option[String], rebuild: Boolean): Option[File]

  def scanFunction(file: File)(code: File => Unit)

  def run(inputDirectory: File, outputDirectory: File, rootPackage: Option[String], rebuild: Boolean) = {

    IO.checkForExistingDirectory(inputDirectory)
    IO.createDirectory(outputDirectory, true)

    var filesCreated: List[File] = Nil

    scanFunction(inputDirectory) {
      process(_, inputDirectory, outputDirectory, rootPackage, rebuild) match {
        case Some(f) => filesCreated = f :: filesCreated
        case None =>
      }
    }

    if (hasError)
      Errors.fatal("Errors detected in source files.")

    logDebug("_ files provided to SBT" << filesCreated.length)

    filesCreated.toList
  }
}
