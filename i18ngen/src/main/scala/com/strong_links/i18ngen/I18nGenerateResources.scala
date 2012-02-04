package com.strong_links.i18ngen

import com.strong_links.core._
import java.io.File

class Generator(runConfig: RunConfig, i18nConfig: I18nConfig, i18nLocale: I18nLocale) extends Logging {

  val poFile = runConfig.getPoFile(i18nConfig, i18nLocale)

  val resourceFile = runConfig.getResourceFile(i18nConfig, i18nLocale)

  def run = Errors.trap(poFile) {

    logDebug("Processing _" << poFile)

    val parseResults = new PoFileReader(poFile, i18nLocale).parse

    val (fuzzyEntries, nonFuzzyEntries) = parseResults.poI18nEntries.partition(_.fuzzy)

    fuzzyEntries.length match {
      case 0 =>
      case n => logWarn(Util.sp("_ fuzzy entry ignored.", "_ fuzzy entries ignored.", n) << n, poFile)
    }

    val (translatedEntries, untranslatedEntries) =
      nonFuzzyEntries.partition(_.translationStatusIsOK(parseResults.headerInfo.nPlural))

    untranslatedEntries.foreach(u => logWarn(u.translationStatus(parseResults.headerInfo.nPlural), poFile))

    new ResourceFileWriter(resourceFile, i18nConfig, i18nLocale, parseResults.headerInfo.nPlural,
      parseResults.headerInfo.pluralForm, translatedEntries).generateAndClose

    logDebug("Generated _" << resourceFile)

    resourceFile
  }
}

object I18nGenerateResources {

  def run(runConfig: RunConfig) =
    runConfig.i18nConfigs.par.flatMap(c => { c.externalI18nLocales.par.map(new Generator(runConfig, c, _).run) }).toList
}

