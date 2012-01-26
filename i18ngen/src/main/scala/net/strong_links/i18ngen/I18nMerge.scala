package net.strong_links.i18ngen

import net.strong_links.core._
import java.io.File

class Merger(runConfig: RunConfig, i18nLocalization: I18nLocalization, scalaI18nCallSummaries: List[ScalaI18nCallSummary])
  extends LoggingPrefixed {

  val loggingPrefixSeq = Seq(i18nLocalization.i18nLanguageKey.string: StringLoggingParameter)

  val poFile = runConfig.getPoFile(i18nLocalization)

  logDebug("Processing _" << poFile)

  def run {

    def parseObsoleteComments(obsoleteComments: List[ObsoletePoComment], i18nLocalization: I18nLocalization) = {
      def tolerantParser(s: String) =
        try new PoReader(s, i18nLocalization).parse.poI18nEntries catch { case _ => Nil }
      obsoleteComments.par.flatMap(oc => tolerantParser(oc.value)).toList
    }

    def fuzzyMatch(scs: ScalaI18nCallSummary, fuzzySource: List[PoI18nEntry]) = {
      val (cost: Double, best) = ((Double.MaxValue, None: Option[PoI18nEntry]) /: fuzzySource.par)((soFar, e) => {
        val c = Util.getWeightedLevenshteinDistance(e.key.compute, scs.key.compute)
        if (c < soFar._1) (c, Some(e))
        soFar
      })
      best match {
        case Some(po) if cost < runConfig.fuzzyThreshold =>
          logInfo("Fuzzy match between !_ and !_" << (scs.key, po.key), "Weighed cost is _" << cost.formatted("%.2f"))
          PoI18nEntry.makeFuzzyFrom(po, scs)
        case _ =>
          PoI18nEntry.makeFrom(scs)
      }
    }

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
    }
  }
}

object I18nMerge extends Logging {

  def invalidCalls(calls: List[ScalaI18nCall])(params: LoggingParameter*) {
    for (c <- calls.sortWith(_.reference < _.reference))
      logError((params :+ new StringLoggingParameter(c.toString)): _*)
    Errors.fatal("Invalid I18n calls.")
  }

  def summarizePackageCalls(packageSegments: List[String], calls: List[ScalaI18nCall]) = {

    val (good, invalid) = calls.groupBy(_.key).toList.partition(_._2.map(_.key.msgidPlural).distinct.length == 1)

    if (invalid != Nil)
      invalidCalls(invalid.flatMap(_._2))("Incompatible plural forms between calls.")

    for ((key, calls) <- good; sortedCalls = calls.sortWith(_.reference < _.reference))
      yield new ScalaI18nCallSummary(key.msgCtxt, key.msgid, key.msgidPlural,
      sortedCalls.flatMap(_.comments), sortedCalls.map(_.reference))
  }

  def distributeCalls(runConfig: RunConfig, calls: List[ScalaI18nCall]) = {

    def emptySet = scala.collection.mutable.Set[ScalaI18nCall]()
    val callsByPackage = runConfig.i18nConfigs.map(_.packageSegments).map(ps => (ps, emptySet)).toMap
    val unknowns = emptySet

    def search(packageSegments: List[String]): scala.collection.mutable.Set[ScalaI18nCall] =
      if (packageSegments == Nil)
        unknowns
      else
        callsByPackage.getOrElse(packageSegments, search(packageSegments.init))

    calls.foreach(c => search(c.packageSegments) += c)

    if (!unknowns.isEmpty)
      invalidCalls(unknowns.toList)("Package is not configured.")

    callsByPackage.map(x => (x._1, summarizePackageCalls(x._1, x._2.toList)))
  }

  def run(runConfig: RunConfig) = {

    val files = IO.scanDirectoryForFileNames(runConfig.inputRootDirectory, _.isExtension("scala"))

    logInfo("Found _ files under _." << (files.length, runConfig.inputRootDirectory))

    val callsByPackage = distributeCalls(runConfig, files.par.flatMap(new ScalaFileReader(_).parse).toList)

    for (c <- runConfig.i18nConfigs) {
      val callsForConfig = callsByPackage.get(c.packageSegments) match {
        case None =>
          Errors.fatal("Calls not found for package _." << c.packageSegments)
        case Some(callSummaries) =>
          // Do an actual merge for master localizations.
          c.masterLocalizations.par.foreach(new Merger(runConfig, _, callSummaries).run)
          // Only do a touch not a run for sublocalizations (create Po file if it does not exist).
          c.subLocalizations.par.foreach(new Merger(runConfig, _, Nil))
      }
    }
  }
}
