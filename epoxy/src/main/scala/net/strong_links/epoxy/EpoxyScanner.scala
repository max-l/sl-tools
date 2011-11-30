package net.strong_links.epoxy

import net.strong_links.core._

import java.io.File

abstract class EpoxyScanner(logger: Xlogger) {

  var hasError = false

  // Note: file names written without backslashes because it makes Eclipse complain
  //       about invalid unicode sequences...
  def header(cs: LeveledCharStream, sourceName: String, destinationFileName: String) {
    def r(s: String) = s.replace("\\", "/")
    val of = r(destinationFileName)
    val sf = r(sourceName)
    // Box has 80 characters wide, or more if needed.
    val pad = 15
    val stars = (80 /: Seq(of, sf))(_ max _.length + pad) - 1
    cs.println("/" + "*" * stars)
    cs.println("*")
    cs.println("* Output file: _" << of)
    cs.println("*")
    cs.println("* Source:      _" << sf)
    cs.println("*")
    cs.println("*" * stars + "/")
    cs.println
  }

  def computePackageNameSegments(file: File, inputDirectory: File, rootPackage: Option[String]) = {
    def ctx = "Package name computation failed for file _, input directory _ and root package _." <<
      (file.getCanonicalPath, inputDirectory.getCanonicalPath, rootPackage)
    try {
      val partialPath = file.getCanonicalPath.substring(inputDirectory.getCanonicalPath.length)
      val segments = partialPath.split(IO.dirSeparator(0)).toList.filter(!_.isEmpty).map(normalizeName(_, ctx))
      if (segments.isEmpty)
        Errors.fatal(ctx, "No segments found in relative path _." << partialPath)
      val results = rootPackage match {
        case None => segments
        case Some(rp) =>
          val leadingSegments = if (rp.contains(".")) rp.split('.').toList else List(rp)
          leadingSegments ::: segments
      }
      if (results.length < 2)
        Errors.fatal(ctx, "Less than two segments in package name _." << results.mkString("."))
      results
    } catch {
      case e: Exception => Errors.fatal(ctx, e)
    }
  }

  def getFileNameWithoutExtension(file: File) = {
    val segments = file.getName.split('.').filter(!_.isEmpty)
    if (segments.length < 2)
      Errors.fatal("Invalid file name _." << file.getCanonicalPath)
    normalizeName(segments.dropRight(1).mkString, "File name _" << file.getCanonicalPath)
  }

  def generateScalaFile[T](entries: Seq[T], outputFile: File, sourceFile: File,
    masterPackageName: String, packageName: String, className: String, objectName: String, objectIsInside: Boolean, imports: List[String])(code: T => String) {
    val cs = new LeveledCharStream
    def genObjectIf(b: Boolean) = if (b) {
      cs.println
      cs.println("package object _ extends _._._" << (objectName, masterPackageName, packageName, className))
    }
    header(cs, sourceFile.getCanonicalPath, outputFile.getCanonicalPath)
    cs.block("package _" << masterPackageName) {
      if (!imports.isEmpty) {
        cs.println
        for (imp <- imports)
          cs.println("import _" << imp)
      }
      cs.block("package _" << packageName) {
        cs.block("class _" << className) {
          entries.foreach(e => cs.println(code(e)))
        }
        genObjectIf(objectIsInside)
      }
      genObjectIf(!objectIsInside)
    }
    IO.createDirectory(outputFile.getParentFile, true)
    IO.writeUtf8ToFile(outputFile, cs.close)
  }

  def process(file: File, inputDirectory: File, outputDirectory: File, rootPackage: Option[String], rebuild: Boolean): Option[File]

  def scanFunction(file: File)(code: File => Unit)

  def run(inputDirectory: File, outputDirectory: File, rootPackage: Option[String], rebuild: Boolean) = {

    IO.checkForExistingDirectory(inputDirectory)
    IO.createDirectory(outputDirectory, true)
    logger.debug("Input directory: _" <<< inputDirectory.getCanonicalPath)
    logger.debug("Output directory: _" <<< outputDirectory.getCanonicalPath)
    logger.debug("Root package:_" <<< (rootPackage match { case None => "(none)"; case Some(x) => x }))
    logger.debug("Rebuild: _" <<< (if (rebuild) "Yes" else "No"))

    val filesCreated = new scala.collection.mutable.ListBuffer[File]

    scanFunction(inputDirectory) {
      process(_, inputDirectory, outputDirectory, rootPackage, rebuild) match {
        case Some(f) => filesCreated += f
        case None =>
      }
    }

    if (hasError)
      throw new Exception("Errors detected in source files.")

    filesCreated.toList
  }
}
