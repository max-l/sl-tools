package net.strong_links.epoxy

import net.strong_links.core._

import java.io.File

class TemplateScanner extends EpoxyScanner with Logging {

  var stackTrace = false

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
    }
  }

  def process(file: File, rootDirectory: File, outputDirectory: File, rootPackage: Option[String], rebuild: Boolean) = {
    val segments = computePackageNameSegments(rootDirectory, file, rootPackage)
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

    logDebug("Processing input file: _" <<< file)
    logDebug("Output file: _" <<< outputFile)
    logDebug("Package: _" <<< fullPackageName)

    if (generate) {
      logDebug("Generating new file _." <<< outputFile)
      Errors.recover {
        generateTemplate(file, outputFile, masterPackageName, packageName, className, objectName)
        Some(outputFile): Option[File]
      } using { e =>
        hasError = true
        IO.deleteFile(outputFile, true)
        None: Option[File]
      }
    } else {
      logDebug("File _ is up-to-date." <<< outputFile)
      Some(outputFile)
    }
  }

  def scanFunction(file: File)(code: File => Unit) = {
    IO.scanDirectory(file)(code(_))
  }
}

