package net.strong_links.epoxy

import net.strong_links.core._

import java.io.File

import SbtInterface._

class TemplateScanner(logger: Logger) extends EpoxyScanner(logger) {

  var stackTrace = false

  def generateTemplate(file: File, outputFile: File, masterPackageName: String, packageName: String, className: String) {
    val entries = new TemplateParser(file, logger).compile
    if (entries.isEmpty)
      IO.deleteFile(outputFile, true)
    else {
      val imports = List("net.strong_links.core._", "net.strong_links.core.Convert._", 
                         "net.strong_links.scalaforms.BaseField", "net.strong_links.scalaforms.OutStream",
                         "import net.strong_links.scalaforms.fieldTransformer", <a/>.getClass.getCanonicalName)
      generateScalaFile(entries, outputFile, file, masterPackageName, packageName, className, imports)(_.code)
    }
  }
  
  def process(file: File, inputDirectory: File, outputDirectory: File, rootPackage: Option[String], rebuild: Boolean) = {
    val segments = computePackageNameSegments(file.getParentFile, inputDirectory, rootPackage)
    val masterPackageSegments = segments.dropRight(1)
    val masterPackageName = masterPackageSegments.mkString(".")
    val packageName = segments.last
    val name = getFileNameWithoutExtension(file)
    val className = name.capitalize
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
    logger.debug("Package: _" <<< packageName)
    
    if (generate) {
      logger.debug("Generating new file _." <<< outputFile.getCanonicalPath)
      try {
        generateTemplate(file, outputFile, masterPackageName, packageName, className)
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

