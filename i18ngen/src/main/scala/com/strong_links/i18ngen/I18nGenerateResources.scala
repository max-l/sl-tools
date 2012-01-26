package com.strong_links.i18ngen

import com.strong_links.core._
import java.io.File

class Generator(runConfig: RunConfig, i18nLocalization: I18nLocalization) extends Logging {

  val poFile = runConfig.getPoFile(i18nLocalization)

  val resourceFile = runConfig.getResourceFile(i18nLocalization)

  def run = {

    logDebug("Processing _" << poFile)

    val parseResults = new PoFileReader(poFile, i18nLocalization).parse

    val (fuzzyEntries, nonFuzzyEntries) = parseResults.poI18nEntries.partition(_.fuzzy)

    fuzzyEntries.length match {
      case 0 =>
      case n => logWarn(Util.sp("_ fuzzy entry ignored.", "_ fuzzy entries ignored.", n) << n, poFile)
    }

    val (translatedEntries, untranslatedEntries) =
      nonFuzzyEntries.partition(_.translationStatusIsOK(parseResults.headerInfo.nPlural))

    untranslatedEntries.foreach(u => logWarn(u.translationStatus(parseResults.headerInfo.nPlural), poFile))

    new ResourceFileWriter(resourceFile, i18nLocalization, parseResults.headerInfo.nPlural,
      parseResults.headerInfo.pluralForm, translatedEntries).generateAndClose

    logDebug("Generated _" << resourceFile)

    resourceFile
  }
}

object I18nGenerateResources {

  def run(runConfig: RunConfig) =
    runConfig.i18nConfigs.par.flatMap(c => { c.allLocalizations.par.map(new Generator(runConfig, _).run) }).toList
}

