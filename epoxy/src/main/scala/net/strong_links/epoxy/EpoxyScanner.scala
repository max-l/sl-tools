package net.strong_links.epoxy

import net.strong_links.core._

import java.io.File

abstract class EpoxyScanner(logger: Xlogger) {

  var hasError = false

  def computePackageNameSegments(file: File, inputDirectory: File, rootPackage: String) = {
    Errors.context("Package name computation failed for file _, input directory _ and root package _." <<
      (file, inputDirectory, rootPackage)) {
      val partialPath = file.getCanonicalPath.substring(inputDirectory.getCanonicalPath.length)
      val segments = Util.split(partialPath, IO.dirSeparator).filter(!_.isEmpty)
      if (segments.isEmpty)
        Errors.fatal("No segments found in relative path _." << partialPath)
      val results = (Util.split(rootPackage, '.') ::: segments).map(Lex.normalizeName(_))
      if (results.length < 2)
        Errors.fatal("Less than two segments in package name _." << results.mkString("."))
      results
    }
  }

  def getFileNameWithoutExtension(file: File) = {
    Errors.context("File name _" << file.getCanonicalPath) {
      val segments = Util.split(file.getName, '.').filter(!_.isEmpty)
      if (segments.length < 2)
        Errors.fatal("Invalid file name _." << file.getCanonicalPath)
      Lex.normalizeName(segments.dropRight(1).mkString)
    }
  }

  def process(file: File, inputDirectory: File, outputDirectory: File, rootPackage: String, rebuild: Boolean): Option[File]

  def scanFunction(file: File)(code: File => Unit)

  def run(inputDirectory: File, outputDirectory: File, rootPackage: String, rebuild: Boolean) = {

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
      throw new Exception("Errors detected in source files.")

    logger.debug("_ files provided to SBT" << filesCreated.length)

    filesCreated.toList
  }
}
