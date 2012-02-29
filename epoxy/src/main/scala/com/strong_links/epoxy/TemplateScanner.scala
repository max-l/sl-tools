package com.strong_links.epoxy

import com.strong_links.core._

import java.io.File

class TemplateScanner extends EpoxyScanner with Logging {

  private def findI18nCatalog(packageNameSegments: List[String], i18nConfigs: Seq[I18nConfig]): Option[String] =
    if (packageNameSegments == Nil)
      None
    else {
      val packageName = packageNameSegments.mkString(".")
      i18nConfigs.find(_.packageName == packageName) match {
        case None    => findI18nCatalog(packageNameSegments.dropRight(1), i18nConfigs)
        case Some(x) => Some(x.packageName)
      }
    }

  def generateTemplate(file: File, outputFile: File, masterPackageName: String, packageName: String,
                       className: String, objectName: String, i18nConfigs: Seq[I18nConfig]) {
    val fullPackageName = masterPackageName + "." + packageName
    val i18nCatalogPackageName = findI18nCatalog(Util.split(fullPackageName, "."), i18nConfigs) match {
      case None     => Errors.fatal("No package matches _ in the i18n configuration." << fullPackageName)
      case Some(ic) => ic + ".i18nCatalog._"
    }
    val (entries, cacheCode) = new TemplateCompiler(file).compile
    if (entries.isEmpty)
      IO.deleteFile(outputFile, true)
    else {
      val imports = List("com.strong_links.core._", "com.strong_links.core.Convert._",
        "com.strong_links.scalaforms.BaseField", "com.strong_links.scalaforms.OutputContext",
        "com.strong_links.scalaforms.TemplateFunction",
        "com.strong_links.scalaforms.StringOutputStream",
        "com.strong_links.scalaforms.fieldTransformer", "com.strong_links.scalaforms.Uri",
        <a/>.getClass.getCanonicalName, i18nCatalogPackageName)
      generateScalaFile(entries, cacheCode, outputFile, file, masterPackageName, packageName, className, objectName, true, imports)(_.code)
      logDebug("Generated file: _." << outputFile)
    }
  }

  def process(file: File, rootDirectory: File, outputDirectory: File, rootPackage: Option[String],
              rebuild: Boolean, i18nConfigs: Seq[I18nConfig]) = {
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
      case None     => ""
      case Some(rp) => Util.split(rp, '.').mkString("", IO.dirSeparator, IO.dirSeparator)
    }
    val objectName = className
    val deltaPath = IO.getRelativePath(rootDirectory, file.getParentFile)
    val outputDirectoryName = outputDirectory.path + IO.dirSeparator + addedPath + deltaPath
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
        generateTemplate(file, outputFile, masterPackageName, packageName, className, objectName, i18nConfigs)
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
    IO.scanDirectory(file, _.isExtension("html", "htm"))(code(_))
  }
}

