package net.strong_links.epoxy

import net.strong_links.core._

import java.io.File

class TemplateScanner extends EpoxyScanner with Logging {

  def generateTemplate(file: File, outputFile: File, masterPackageName: String, packageName: String,
    className: String, objectName: String) {
    val entries = new TemplateParser(file).compile
    if (entries.isEmpty)
      IO.deleteFile(outputFile, true)
    else {
      val imports = List("net.strong_links.core._", "net.strong_links.core.Convert._",
        "net.strong_links.scalaforms.BaseField", "net.strong_links.scalaforms.OutStream",
        "net.strong_links.scalaforms.fieldTransformer", <a/>.getClass.getCanonicalName)
      generateScalaFile(entries, outputFile, file, masterPackageName, packageName, className, objectName, true, imports)(_.code)
      logDebug("Generated file: _." << outputFile)
    }
  }

  def process(file: File, rootDirectory: File, outputDirectory: File, rootPackage: Option[String], rebuild: Boolean) = {
    logDebug("Processing template _" <<< file)
    val segments = computePackageNameSegments(rootDirectory, file, rootPackage)
    if (segments.length < 2)
      Errors.fatal("Not enough segments in _, expected at least 2." << segments)
    val fullPackageName = segments.mkString(".")
    val masterPackageSegments = segments.dropRight(1)
    val masterPackageName = masterPackageSegments.mkString(".")
    val packageName = segments.last
    val className = {
      val x = getFileNameWithoutExtension(file)
      if (x(0).isUpper)
        x(0).toLower + x.substring(1)
      else
        x
    }
    val addedPath = rootPackage match {
      case None => ""
      case Some(rp) => Util.split(rp, '.').mkString("", IO.dirSeparator, IO.dirSeparator)
    }
    val objectName = className
    val deltaPath = IO.getRelativePath(rootDirectory, file.getParentFile)
    val outputDirectoryName = outputDirectory.getCanonicalPath + IO.dirSeparator + addedPath + deltaPath
    IO.createDirectory(new File(outputDirectoryName), true)
    val outputFile = new File(outputDirectoryName + IO.dirSeparator + className + ".scala")
    val generate =
      if (outputFile.exists && !rebuild)
        file.lastModified > outputFile.lastModified
      else
        true
    if (generate) {
      logDebug("Output file must be generated.")
      try {
        generateTemplate(file, outputFile, masterPackageName, packageName, className, objectName)
        Some(outputFile): Option[File]
      } catch {
        case e =>
          logError(e, false)
          hasError = true
          IO.deleteFile(outputFile, true)
          None
      }
    } else {
      logDebug("Output file is already up-to-date - not generated.")
      Some(outputFile)
    }
  }

  def scanFunction(file: File)(code: File => Unit) = {
    IO.scanDirectory(file)(code(_))
  }
}

