package net.strong_links.epoxy

import net.strong_links.core._

import java.io.File

class TemplateScanner(logger: Xlogger) extends EpoxyScanner(logger) {

  var stackTrace = false

  def generateTemplate(file: File, outputFile: File, masterPackageName: String, packageName: String,
    className: String, objectName: String) {
    val entries = new TemplateParser(file, logger).compile
    if (entries.isEmpty)
      IO.deleteFile(outputFile, true)
    else {
      val imports = List("net.strong_links.core._", "net.strong_links.core.Convert._",
        "net.strong_links.scalaforms.BaseField", "net.strong_links.scalaforms.OutStream",
        "net.strong_links.scalaforms.fieldTransformer", <a/>.getClass.getCanonicalName)
      generateScalaFile(entries, outputFile, file, masterPackageName, packageName, className, objectName, true, imports)(_.code)
    }
  }

  def process(file: File, inputDirectory: File, outputDirectory: File, rootPackage: Option[String], rebuild: Boolean) = {
    val segments = computePackageNameSegments(file.getParentFile, inputDirectory, rootPackage)
    val fullPackageName = segments.mkString(".")
    val masterPackageSegments = segments.dropRight(1)
    val masterPackageName = masterPackageSegments.mkString(".")
    val packageName = segments.last
    val objectName = {
      val x = getFileNameWithoutExtension(file)
      if (x(0).isUpper)
        x(0).toLower + x.substring(1)
      else
        x
    }
    val className = objectName.capitalize
    val outputDirectoryName = outputDirectory.getCanonicalPath + IO.dirSeparator + (segments.mkString(IO.dirSeparator))
    IO.createDirectory(new File(outputDirectoryName), true)
    val outputFile = new File(outputDirectoryName + IO.dirSeparator + className + ".scala")

    val generate =
      if (outputFile.exists && !rebuild)
        file.lastModified > outputFile.lastModified
      else
        true

    logger.debug("Processing input file: _" <<< file.getCanonicalPath)
    logger.debug("Output file: _" <<< outputFile.getCanonicalPath)
    logger.debug("Package: _" <<< fullPackageName)

    if (generate) {
      logger.debug("Generating new file _." <<< outputFile.getCanonicalPath)
      try {
        generateTemplate(file, outputFile, masterPackageName, packageName, className, objectName)
        Some(outputFile)
      } catch {
        case e: LexError =>
          hasError = true
          IO.deleteFile(outputFile, true)
          None
        case e: Exception =>
          Errors.fatal(e)
      }
    } else {
      logger.debug("File _ is up-to-date." <<< outputFile.getCanonicalPath)
      None
    }
  }

  def scanFunction(file: File)(code: File => Unit) = {
    IO.scanDirectory(file)(code(_))
  }
}

