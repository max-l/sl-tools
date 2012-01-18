package net.strong_links.i18ngen

import net.strong_links.core._

class I18nGenMerge(runConfig: RunConfig) extends I18nGen(runConfig) {

  def makeScalaI18nCallSummaries(scalaI18nCalls: List[ScalaI18nCall]) = {
    def refs(calls: List[ScalaI18nCall]) = calls.map(_.reference).sorted
    I18nKey.group(scalaI18nCalls)(calls => "Referenced at _." << refs(calls)) {
      (msgCtxt, msgid, msgidPlural, calls) =>
        new ScalaI18nCallSummary(msgCtxt, msgid, msgidPlural, calls.flatMap(_.comments), refs(calls))
    }.sorted
  }

  def mergeLocalization(i18nLocalization: I18nLocalization, scalaI18nCallSummaries: List[ScalaI18nCallSummary]) {

    val poFile = poFileFor(i18nLocalization)

    if (!poFile.exists) {
      IO.writeUtf8ToFile(poFile, PoFileHeader.makeDefault(i18nLocalization))
      logInfo("_ created with default contents." << poFile)
    }

    Errors.trap("_" << poFile) {

      val parseResults = new PoFileReader(poFile).parse

      val fileHeader = new PoFileHeader(parseResults.poHeaderEntry, i18nLocalization)

      val translatedPoEntries = parseResults.poI18nEntries.filter(_.translations.exists(!_.isEmpty))

      println("Translated entries: _" << translatedPoEntries)
    }
  }

  def merge = {

    val i18nCallsInScalaFiles = {
      val b = scala.collection.mutable.ListBuffer[ScalaI18nCall]()
      IO.scanDirectory(runConfig.inputDirectory, _.isExtension("scala")) { f =>
        new ScalaFileReader(f, b).parse
      }
      makeScalaI18nCallSummaries(b.toList)
    }

    runConfig.masterLocalizations.foreach(mergeLocalization(_, i18nCallsInScalaFiles))
  }
}

