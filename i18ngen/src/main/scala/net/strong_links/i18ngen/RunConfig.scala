package net.strong_links.i18ngen

import net.strong_links.core._
import net.strong_links.core.codegen._

import java.io.File

class RunConfig(packageName: String, languageKey: String, localizationsStr: String, val fuzzyThreshold: Double,
  val inputRootDirectory: File, val outputRootDirectory: File)
  extends I18nConfig(packageName, languageKey, localizationsStr) with Logging with CodeGeneration {

  logDebug("Package name: _" << packageName)
  logDebug("Code localization: _" << codeLocalization)
  logDebug("Localizations: _" << allLocalizations)

  private def make(rootdir: File) = new File(rootdir.path + IO.dirSeparator + packageSegments.mkString(IO.dirSeparator))

  val inputDirectory = make(inputRootDirectory)
  val outputDirectory = make(outputRootDirectory)

  logDebug("Input: _" << inputDirectory)
  logDebug("Output: _" << outputDirectory)

  IO.checkForExistingDirectory(inputDirectory)
  IO.createDirectory(outputDirectory, true)

  def generateCatalog = {
    def cloc(i18nCodeLocalization: I18nCodeLocalization): String = {
      val lk = i18nCodeLocalization.i18nLanguageKey.string
      "    val _ = I18nCodeLocalization._(pName)" << (lk, lk)
    }
    def loc(i18nLocalization: I18nLocalization): String = {
      val pl: String = i18nLocalization.parent match {
        case None => "None"
        case Some(x) => "Some(_)" << x.i18nLanguageKey.string
      }
      val lk = i18nLocalization.i18nLanguageKey.string
      "    val _ = new I18nLocalization(pName, I18nLanguageKey.from(\"_\"), _)" << (lk, lk, pl)
    }
    val outputFile = new File(outputDirectory.path + IO.dirSeparator + "PackageI18nConfig.scala")
    logDebug("Generating catalog _." << outputFile)
    val b = scala.collection.mutable.ListBuffer[String]()
    b += ("package " + packageName)
    b += ("")
    b += ("import net.strong_links.core._")
    b += ("")
    b += ("object PackageI18nConfig {")
    val x = codeLocalization.i18nLanguageKey.string
    val y = allLocalizations.map(_.toString).mkString(",")
    b += ("  def catalog = new I18nConfig(\"_\", \"_\", \"_\").toCatalog" << (packageName, x, y))
    b += ("}")
    IO.writeUtf8ToFile(outputFile, b.mkString("\n"))
  }
}
