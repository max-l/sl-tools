package net.strong_links.i18ngen

import net.strong_links.core._
import net.strong_links.core.codegen._

import java.io.File

class RunConfig(val packageName: String, codeLanguage: String, localizationsStr: String, val inputRootDirectory: File, val outputRootDirectory: File) extends Logging with CodeGeneration {

  class info(val language: String, val country: Option[String], val parent: Option[String], val originalData: String)

  val codeLocalization = I18nCodeLocalization.apply(packageName, codeLanguage)

  val (masters, subs) = splitLocalizations.partition(_.parent == None)

  val masterLocalizations = masters.map(m => new I18nLocalization(packageName, m.language, m.country, None))

  val subLocalizations = {
    val allMasterLocalizations = codeLocalization :: masterLocalizations
    subs.map(s =>
      s.parent match {
        case None => Errors.fatal("No parent for sub-localization.")
        case Some(ps) =>
          allMasterLocalizations.find(_.language == ps) match {
            case None => Errors.fatal("Master localization _ not found for sublocalization _." << (ps, s.originalData))
            case Some(m) => new I18nLocalization(packageName, s.language, s.country, Some(m))
          }
      })
  }

  val localizations =
    masterLocalizations ::: subLocalizations

  I18nUtil.checkUniqueness(codeLocalization, localizations)

  logDebug("Package name: _" << packageName)
  logDebug("Code localization: _" << codeLocalization)
  logDebug("Localizations: _" << localizations)

  val segments = Util.split(packageName, '.')
  checkPackageSegments(segments)

  private def make(rootdir: File) = new File(rootdir.getCanonicalPath + IO.dirSeparator + segments.mkString(IO.dirSeparator))

  val inputDirectory = make(inputRootDirectory)
  val outputDirectory = make(outputRootDirectory)

  logDebug("Input directory: _" << inputDirectory)
  logDebug("Output directory: _" << outputDirectory)

  IO.checkForExistingDirectory(inputDirectory)
  IO.createDirectory(outputDirectory, true)

  def splitLocalizations = Util.split(localizationsStr, ",").map(_.trim).filter(!_.isEmpty).map { s =>
    Errors.trap("Invalid language/variant specification _" << s) {
      def split(s: String, on: Char) = if (s.contains(on)) Util.splitTwo(s, on) else (s, "")
      val (id, parentLanguage) = split(s, ':')
      val (language, country) = split(id, '_')
      def check(s: String, f: String => String, what: String): Option[String] = {
        val st = s.trim
        if (st == "")
          None
        else {
          if (st.length != 2)
            Errors.fatal("Invalid _ code _ (not two characters)." << (what, st))
          Some(f(st))
        }
      }
      def toLanguage = check(language, _.toLowerCase, "language")
      def toCountry = check(country, _.toUpperCase, "country")
      def toParentLanguage = check(parentLanguage, _.toLowerCase, "parent language")
      toLanguage match {
        case None => Errors.fatal("Missing language code.")
        case Some(language) => new info(language, toCountry, toParentLanguage, s)
      }
    }
  }
}
