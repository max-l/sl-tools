package net.strong_links.i18ngen

import net.strong_links.core._
import java.io.File

class Merger(runConfig: RunConfig, i18nLocalization: I18nLocalization, scalaI18nCallSummaries: List[ScalaI18nCallSummary])
  extends LocalizationRunner(runConfig, i18nLocalization) {

  def run {

    def parseObsoleteComments(obsoleteComments: List[ObsoletePoComment], i18nLocalization: I18nLocalization) = {
      def tolerantParser(s: String) =
        try new PoReader(s, i18nLocalization).parse.poI18nEntries catch { case _ => Nil }
      obsoleteComments.par.flatMap(oc => tolerantParser(oc.value)).toList
    }

    def fuzzyMatch(scs: ScalaI18nCallSummary, fuzzySource: List[PoI18nEntry]) = {
      val (cost: Double, best) = ((Double.MaxValue, None: Option[PoI18nEntry]) /: fuzzySource.par)((soFar, e) => {
        val c = Util.getWeightedLevenshteinDistance(e.key.compute, scs.key.compute)
        if (c < soFar._1) (c, Some(e)) else soFar
      })
      best match {
        case Some(po) if cost < runConfig.fuzzyThreshold =>
          logInfo("Fuzzy match between !_ and !_" << (scs.key, po.key), "Weighed cost is _" << cost.formatted("%.2f"))
          PoI18nEntry.makeFuzzyFrom(po, scs)
        case _ =>
          PoI18nEntry.makeFrom(scs)
      }
    }

    logInfo("Processing _" << poFile)

    Errors.trap(poFile) {

      // Parse the Po file.
      val parseResults = new PoFileReader(poFile, i18nLocalization).parse

      // Also parse its obsolete entries, accepting duplicate values, etc.
      val oldObsoleteEntries = parseObsoleteComments(parseResults.obsoleteComments, i18nLocalization)

      // Get the old Po entries that have some translations and that are non fuzzy. These are considered valuable..
      val oldPoEntries = parseResults.poI18nEntries.filter(po => po.translations.exists(!_.isEmpty) && !po.fuzzy)
      val oldPoEntriesMap = PoI18nEntry.toMap(oldPoEntries)

      // Use all previous non fuzzy Po entries as a source for fuzzy matches, regardless of their uniqueness.
      val fuzzySource = (oldPoEntries ::: oldObsoleteEntries)

      // Compute the new Po entries.
      val p = scalaI18nCallSummaries.partition(scs => oldPoEntriesMap.contains(scs.key))
      val mergedPoEntries = p._1.par.map(scs => PoI18nEntry.merge(scs, oldPoEntriesMap.get(scs.key).head)).toList
      val createdPoEntries = p._2.map(scs => fuzzyMatch(scs, fuzzySource))
      val newPoEntries = (mergedPoEntries ::: createdPoEntries).sorted
      val newUnFuzzyPoEntriesMap = PoI18nEntry.toMap(newPoEntries.filter(!_.fuzzy))

      val newObsoleteComments = fuzzySource.filter(po => !newUnFuzzyPoEntriesMap.contains(po.key)).map(e =>
        new ObsoletePoComment(e.generate(parseResults.headerInfo.nPlural)))

      // Create the new Po file containing the new and recovered strings.
      val newPoFile = IO.createTemporaryFile
      val freshPoFileWriter = new PoFileWriter(newPoFile, parseResults.headerInfo.nPlural, parseResults.poHeaderEntry :: newPoEntries, newObsoleteComments)
      val k = freshPoFileWriter.generate
      logWarn(Util.sp("_ entry is missing translations.", "_ entries are missing translations.", k) << k)

      // The newly merged Po file can now replace the old Po file.
      logDebug("Renaming _ to _" <<< (newPoFile, poFile))
      IO.deleteFile(poFile)
      IO.renameFile(newPoFile, poFile)

      logInfo("Done")
    }
  }
}

object I18nMerge extends Logging {

  def run(runConfig: RunConfig) = {

    val i18nCallsInScalaFiles = {
      val files = IO.scanDirectoryNames(runConfig.inputDirectory, _.isExtension("scala", "java"))
      logInfo("Scanning _ for I18n calls (_ files)" << (runConfig.inputDirectory, files.length))
      val callsFound = files.par.flatMap(new ScalaFileReader(_).parse).toList
      for (c <- callsFound)
        println("_ in _" << (c.key, c.pack))
      val summaries = for (
        (key, calls) <- callsFound.groupBy(_.key);
        sortedCalls = calls.sortWith(_.reference < _.reference)
      ) yield {
        def refs = sortedCalls.map(_.reference)
        sortedCalls.map(_.key.msgidPlural).distinct match {
          case List(msgidPlural) =>
            new ScalaI18nCallSummary(key.msgCtxt, key.msgid, msgidPlural, sortedCalls.flatMap(_.comments), refs)
          case msgidPlurals =>
            Errors.fatal("The I18n key !_ has these incompatible plural forms: !_" << (key, msgidPlurals),
              "Referenced at !_." << refs)
        }
      }
      summaries.toList
    }

    runConfig.masterLocalizations.par.foreach {
      localization =>
        new Merger(runConfig, localization, i18nCallsInScalaFiles).run
    }
  }
}

