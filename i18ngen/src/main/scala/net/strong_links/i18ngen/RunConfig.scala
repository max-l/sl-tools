package net.strong_links.i18ngen

import net.strong_links.core._
import net.strong_links.core.codegen._

import java.io.File

class RunConfig(val packageName: String, languageKey: String, localizationsStr: String, val inputRootDirectory: File, val outputRootDirectory: File) extends Logging with CodeGeneration {

  class Info(val i18nLanguageKey: I18nLanguageKey, val parentI18nLanguageKey: Option[I18nLanguageKey])

  val codeLocalization = I18nCodeLocalization(packageName, I18nLanguageKey.from(languageKey))

  val infos = Util.split(localizationsStr, ",").map(_.trim).filter(!_.isEmpty).map { s =>
    Errors.trap("Invalid language/variant specification _" << s) {
      val (i18nLanguageKey, parentI18nLanguageKey) = if (s.contains(':')) {
        val (lk, plk) = Util.splitTwo(s, ':')
        (I18nLanguageKey.from(lk), Some(I18nLanguageKey.from(plk)))
      } else
        (I18nLanguageKey.from(s), None)
      new Info(i18nLanguageKey, parentI18nLanguageKey)
    }
  }

  val (masterInfos, subInfos) = infos.partition(_.parentI18nLanguageKey == None)

  val masterLocalizations = masterInfos.map(m => new I18nLocalization(packageName, m.i18nLanguageKey, None))

  val subLocalizations = {
    val allMasterLocalizations = codeLocalization :: masterLocalizations
    subInfos.map(s =>
      s.parentI18nLanguageKey match {
        case Some(plk) =>
          allMasterLocalizations.find(_.i18nLanguageKey == plk) match {
            case None => Errors.fatal("Master localization _ not found for sublocalization _." << (plk, s.i18nLanguageKey))
            case Some(m) => new I18nLocalization(packageName, s.i18nLanguageKey, Some(m))
          }
        case None => Errors.fatal("No parent for sub-localization.")
      })
  }

  val allLocalizations = masterLocalizations ::: subLocalizations

  I18nUtil.checkUniqueness(codeLocalization, allLocalizations)

  logDebug("Package name: _" << packageName)
  logDebug("Code localization: _" << codeLocalization)
  logDebug("Localizations: _" << allLocalizations)

  val segments = Util.split(packageName, '.')
  checkPackageSegments(segments)

  private def make(rootdir: File) = new File(rootdir.path + IO.dirSeparator + segments.mkString(IO.dirSeparator))

  val inputDirectory = make(inputRootDirectory)
  val outputDirectory = make(outputRootDirectory)

  logDebug("Input directory: _" << inputDirectory)
  logDebug("Output directory: _" << outputDirectory)

  IO.checkForExistingDirectory(inputDirectory)
  IO.createDirectory(outputDirectory, true)
}
