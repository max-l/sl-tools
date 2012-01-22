package net.strong_links.i18ngen

import net.strong_links.core._
import java.io.File

class I18nGenGenerate(runConfig: RunConfig) extends I18nGen(runConfig) {

  class Generator(i18nLocalization: I18nLocalization) extends LocalizationRunner(i18nLocalization) {

    def run = {

      val poFile = poFileFor(i18nLocalization)

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

      val resourceFile = resourceFileFor(i18nLocalization)

      new ResourceFileWriter(resourceFile, i18nLocalization, parseResults.headerInfo.nPlural,
        parseResults.headerInfo.pluralForm, translatedEntries).generateAndClose

      logInfo("Generated _" << resourceFile)
    }
  }

  def generate =
    runConfig.allLocalizations.par.foreach(new Generator(_).run)
}

