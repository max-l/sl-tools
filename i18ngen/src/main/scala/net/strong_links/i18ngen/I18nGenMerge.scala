package net.strong_links.i18ngen

import net.strong_links.core._
import java.io.File

class I18nGenMerge(runConfig: RunConfig) extends I18nGen(runConfig) {

  private def parseObsoleteComments(obsoleteComments: List[ObsoletePoComment], i18nLocalization: I18nLocalization) = {
    def tolerantParser(s: String) =
      try new PoReader(s, i18nLocalization).parse.poI18nEntries catch { case _ => Nil }
    obsoleteComments.flatMap(oc => tolerantParser(oc.value))
  }

  private def fuzzyMatch(scs: ScalaI18nCallSummary, fuzzySource: List[PoI18nEntry]) = {
    val WEIGHTED_COST_THRESHOLD = 0.3
    val (cost: Double, best) = ((0.0, None: Option[PoI18nEntry]) /: fuzzySource)((soFar, e) => {
      val c = Util.getWeightedLevenshteinDistance(e.key.compute, scs.key.compute)
      if (c < soFar._1) (c, Some(e)) else soFar
    })
    best match {
      case Some(po) if cost < WEIGHTED_COST_THRESHOLD => logInfo("Fuzzy match between _ and _" << (scs.key, po.key)); PoI18nEntry.makeFuzzyFrom(po, scs)
      case _ => PoI18nEntry.makeFrom(scs)
    }
  }

  def mergeLocalization(i18nLocalization: I18nLocalization, scalaI18nCallSummaries: List[ScalaI18nCallSummary]) {

    val poFile = poFileFor(i18nLocalization)

    logInfo("Merging for language _, _." << (i18nLocalization.i18nLanguageKey.string, poFile))

    if (!poFile.exists) {
      IO.writeUtf8ToFile(poFile, PoHeaderInfo.makeDefault(i18nLocalization))
      logInfo("_ created with default contents." << poFile)
    }

    Errors.trap("_" << poFile) {

      // Parse the Po file.
      val parseResults = new PoFileReader(poFile, i18nLocalization).parse

      // Also parse its obsolete entries, accepting duplicate values, etc.
      val oldObsoleteEntries = parseObsoleteComments(parseResults.obsoleteComments, i18nLocalization)

      // Get the old Po entries that have some translations.
      val oldPoEntries = parseResults.poI18nEntries.filter(_.translations.exists(!_.isEmpty))
      val oldPoEntriesMap = PoI18nEntry.toMap(oldPoEntries)

      // Use all previous non fuzzy Po entries as a source for fuzzy matches, regardless of their uniqueness.
      val fuzzySource = (oldPoEntries ::: oldObsoleteEntries).filter(!_.fuzzy)

      // Compute the new Po entries.
      val p = scalaI18nCallSummaries.partition(scs => oldPoEntriesMap.contains(scs.key))
      val mergedPoEntries = p._1.map(scs => PoI18nEntry.merge(scs, oldPoEntriesMap.get(scs.key).head))
      val createdPoEntries = p._2.map(scs => fuzzyMatch(scs, fuzzySource))
      val newPoEntries = (mergedPoEntries ::: createdPoEntries).sorted
      val newPoEntriesMap = PoI18nEntry.toMap(newPoEntries)

      val newObsoleteComments = fuzzySource.filter(po => !newPoEntriesMap.contains(po.key)).map(e =>
        new ObsoletePoComment(e.generate(parseResults.headerInfo.nPlural)))

      // Create the new Po file containing the new and recovered strings.
      val newPoFile = IO.createTemporaryFile
      val freshPoFileWriter = new PoFileWriter(newPoFile, parseResults.headerInfo.nPlural, parseResults.poHeaderEntry :: newPoEntries, newObsoleteComments)
      freshPoFileWriter.generate

      // The newly merged Po file can now replace the old Po file.
      logDebug("Renaming _ to _" <<< (newPoFile, poFile))
      IO.deleteFile(poFile)
      IO.renameFile(newPoFile, poFile)
    }
  }

  def merge = {

    val i18nCallsInScalaFiles = {
      val b = scala.collection.mutable.ListBuffer[ScalaI18nCall]()
      IO.scanDirectory(runConfig.inputDirectory, _.isExtension("scala")) { f => new ScalaFileReader(f, b).parse }
      (for (
        (key, calls) <- b.toList.groupBy(_.key);
        sortedCalls = calls.sortWith(_.reference < _.reference)
      ) yield {
        def refs = sortedCalls.map(_.reference)
        sortedCalls.map(_.key.msgidPlural).distinct match {
          case List(_) =>
            new ScalaI18nCallSummary(key.msgCtxt, key.msgid, key.msgidPlural, sortedCalls.flatMap(_.comments), refs)
          case msgidPlurals =>
            Errors.fatal("The I18n key !_ has these incompatible plural forms: !_" << (key, msgidPlurals),
              "Referenced at !_." << refs)
        }
      }).toList
    }

    runConfig.masterLocalizations.foreach(mergeLocalization(_, i18nCallsInScalaFiles))
  }
}

