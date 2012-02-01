package com.strong_links.i18ngen

import com.strong_links.core._
import com.strong_links.core.codegen._

import java.io.File

object RunConfig {

  val DEFAULT_FUZZY_THRESHOLD = 0.3

  private def toPps(packageSpecifications: String) = {
    Util.split(packageSpecifications, "/") match {
      case List(codeKey) => (codeKey, "", "")
      case List(codeKey, masterKeys) => (codeKey, masterKeys, "")
      case List(codeKey, masterKeys, subKeys) => (codeKey, masterKeys, subKeys)
      case _ => Errors.fatal("Invalid package specifications _." << packageSpecifications)
    }
  }

  def toI18nConfigs(specifications: String) =
    for (
      s <- Util.split(specifications, ';').map(_.trim).filter(!_.isEmpty);
      (packageName, packageSpecifications) = Util.splitTwoTrimmed(s, '=');
      (codeKey, masterKeys, subKeys) = toPps(packageSpecifications)
    ) yield new I18nConfig(packageName, codeKey, masterKeys, subKeys)
}

class RunConfig(val i18nConfigs: Seq[I18nConfig], val optionalFuzzyThreshold: Option[Double], val inputRootDirectory: File, val outputRootDirectory: File)
  extends Logging {

  val fuzzyThreshold = optionalFuzzyThreshold.getOrElse(RunConfig.DEFAULT_FUZZY_THRESHOLD)

  if (fuzzyThreshold < 0)
    Errors.fatal("Fuzzy match threshold _ cannot be negative." << fuzzyThreshold)

  i18nConfigs.groupBy(_.packageName).filter(_._2.length > 1).map(_._1).toList match {
    case Nil =>
    case badGuys => Errors.fatal("Duplicate packages _." << badGuys)
  }

  def this(specifications: String, optionalFuzzyThreshold: Option[Double], inputRootDirectory: File, outputRootDirectory: File) =
    this(RunConfig.toI18nConfigs(specifications), optionalFuzzyThreshold, inputRootDirectory, outputRootDirectory)

  logDebug("Input root: _" << inputRootDirectory)
  IO.checkForExistingDirectory(inputRootDirectory)

  private def getFileFor(rootDir: File, packageName: String, fileName: String) = {
    val dir = new File(rootDir.path + IO.dirSeparator + packageName.replace(".", IO.dirSeparator))
    IO.createDirectory(dir, true)
    val fname = dir.path + IO.dirSeparator + fileName
    new File(fname)
  }

  def getOutputFileFor(packageName: String, fileName: String) = getFileFor(outputRootDirectory, packageName, fileName)

  def getInputFileFor(packageName: String, fileName: String) = getFileFor(inputRootDirectory, packageName, fileName)

  def getInputClassFile(i18nConfig: I18nConfig, i18nConfigLocalization: I18nConfigLocalization, extension: String) = {
    val fname = i18nConfigLocalization.classNameFor(i18nConfig.packageNameSegments) + "." + extension
    getInputFileFor(i18nConfig.packageName, fname)
  }

  def getOutputClassFile(i18nConfig: I18nConfig, i18nConfigLocalization: I18nConfigLocalization, extension: String) = {
    val fname = i18nConfigLocalization.classNameFor(i18nConfig.packageNameSegments) + "." + extension
    getOutputFileFor(i18nConfig.packageName, fname)
  }

  def getPoFile(i18nConfig: I18nConfig, i18nConfigLocalization: I18nConfigLocalization) = {
    val poFile = getInputClassFile(i18nConfig, i18nConfigLocalization, "po")
    if (!poFile.exists) {
      IO.writeUtf8ToFile(poFile, PoHeaderInfo.makeDefault(i18nConfig, i18nConfigLocalization))
      logInfo("Created default _." << poFile)
    }
    poFile
  }

  def getResourceFile(i18nConfig: I18nConfig, i18nConfigLocalization: I18nConfigLocalization) =
    getOutputClassFile(i18nConfig, i18nConfigLocalization, "scala")
}
