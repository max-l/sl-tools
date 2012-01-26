package net.strong_links.i18ngen

import net.strong_links.core._
import net.strong_links.core.codegen._

import java.io.File

object RunConfig {
  def toI18nConfigs(specifications: String) =
    for (
      s <- Util.split(specifications, ';').map(_.trim).filter(!_.isEmpty);
      (packageName, packageSpecifications) = Util.splitTwoTrimmed(s, '=');
      (codeLanguageKey, localizationsStr) = if (packageSpecifications.contains("/"))
        Util.splitTwoTrimmed(packageSpecifications, '/')
      else
        (packageSpecifications, "")
    ) yield new I18nConfig(packageName, codeLanguageKey, localizationsStr)
}

class RunConfig(val i18nConfigs: List[I18nConfig], val fuzzyThreshold: Double, val inputRootDirectory: File, val outputRootDirectory: File)
  extends Logging {

  i18nConfigs.groupBy(_.packageName).filter(_._2.length > 1).map(_._1) match {
    case Nil =>
    case badGuys => Errors.fatal("Duplicate packages _." << badGuys)
  }

  def this(specifications: String, fuzzyThreshold: Double, inputRootDirectory: File, outputRootDirectory: File) =
    this(RunConfig.toI18nConfigs(specifications), fuzzyThreshold, inputRootDirectory, outputRootDirectory)

  logDebug("Input root: _" << inputRootDirectory)
  IO.checkForExistingDirectory(inputRootDirectory)

  private def getFileFor(rootDir: File, packageName: String, fileName: String) = {
    val dir = new File(rootDir.path + IO.dirSeparator + packageName.replace(".", IO.dirSeparator))
    IO.createDirectory(dir, true)
    new File(dir.path + IO.dirSeparator + fileName)
  }

  def getOutputFileFor(packageName: String, fileName: String) = getFileFor(outputRootDirectory, packageName, fileName)

  def getInputFileFor(packageName: String, fileName: String) = getFileFor(inputRootDirectory, packageName, fileName)

  def getPoFile(i18nLocalization: I18nLocalization) = {
    val poFile = getInputFileFor(i18nLocalization.packageName, i18nLocalization.className + ".po")
    if (!poFile.exists) {
      IO.writeUtf8ToFile(poFile, PoHeaderInfo.makeDefault(i18nLocalization))
      logInfo("Created default _." << poFile)
    }
    poFile
  }

  def getResourceFile(i18nLocalization: I18nLocalization) =
    getOutputFileFor(i18nLocalization.packageName, i18nLocalization.className + ".scala")
}
