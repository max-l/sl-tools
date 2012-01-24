package net.strong_links.i18ngen

import net.strong_links.core._
import java.io.File

class Generator(runConfig: RunConfig, i18nLocalization: I18nLocalization)
  extends LocalizationRunner(runConfig, i18nLocalization) {

  def run = {

    logInfo("Processing _" << poFile)

    val parseResults = new PoFileReader(poFile, i18nLocalization).parse

    val (fuzzyEntries, nonFuzzyEntries) = parseResults.poI18nEntries.partition(_.fuzzy)

    fuzzyEntries.length match {
      case 0 =>
      case n => logWarn(Util.sp("_ fuzzy entry ignored.", "_ fuzzy entries ignored.", n) << n)
    }

    val (translatedEntries, untranslatedEntries) =
      nonFuzzyEntries.partition(_.translationStatusIsOK(parseResults.headerInfo.nPlural))

    untranslatedEntries.foreach(u => logWarn(u.translationStatus(parseResults.headerInfo.nPlural)))

    new ResourceFileWriter(resourceFile, i18nLocalization, parseResults.headerInfo.nPlural,
      parseResults.headerInfo.pluralForm, translatedEntries).generateAndClose

    logInfo("Generated _" << resourceFile)

    resourceFile
  }
}

object I18nGenerateResources {

  def run(runConfig: RunConfig) = runConfig.allLocalizations.par.map {
    localization =>
      new Generator(runConfig, localization).run
  }.toList
}

