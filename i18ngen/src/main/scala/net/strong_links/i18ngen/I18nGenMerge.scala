package net.strong_links.i18ngen

import net.strong_links.core._

class I18nGenMerge(runConfig: RunConfig) extends I18nGen(runConfig) {

  def makeScalaI18nCallSummaries(scalaI18nCalls: List[ScalaI18nCall]) = {
    def refs(calls: List[ScalaI18nCall]) = calls.map(_.reference).sorted

    (for ((key, entries) <- scalaI18nCalls.groupBy(_.key)) yield {
      def refs = entries.map(_.reference).sorted
      entries.map(_.key.msgidPlural).distinct match {
        case List(_) =>
          new ScalaI18nCallSummary(key.msgCtxt, key.msgid, key.msgidPlural, entries.flatMap(_.comments), refs)
        case msgidPlurals =>
          Errors.fatal("The I18n key !_ has these incompatible plural forms: !_" << (key, msgidPlurals),
            "Referenced at !_." << refs)
      }
    }).toList.sorted
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

      // Get the Po entries that have some translations.
      val translatedPoEntries = parseResults.poI18nEntries.filter(_.translations.exists(!_.isEmpty))

      // Check if there are duplicate entries in the PO entries
      translatedPoEntries.groupBy(_.key).filter(_._2.length > 1).map(_._1.computeForHuman) match {
        case Nil =>
        case dups => Errors.fatal("Duplicate entries found: !_" << dups)
      }

      val outputPoEntries = scala.collection.mutable.ListBuffer[PoI18nEntry]()

      // Create a fast access map for the translated Po entries.
      val poEntries = (scala.collection.mutable.Map[I18nKey, PoI18nEntry]() /: translatedPoEntries)((m, e) => m += (e.key -> e))

      println(poEntries)

      // Process each Scala I18n entry
      for (e <- scalaI18nCallSummaries) {
        val newEntry = if (poEntries.contains(e.key)) {
          // The Scala entry has been found in the Po entries map, so this is an exact match. Merge the data from the
          // Po and Scala entries together.
          val po = poEntries(e.key)
          new PoI18nEntry(e.key.msgCtxt, e.key.msgid, e.key.msgidPlural, po.comments ::: e.comments, po.translations, e.references, false)
        } else
          // Else it is just a new entry found in Scala files.
          new PoI18nEntry(e.key.msgCtxt, e.key.msgid, e.key.msgidPlural, e.comments, Nil, e.references, false)
        outputPoEntries += newEntry
      }

      println("OUTPUT")
      println(outputPoEntries)

      val finalEntries = parseResults.poHeaderEntry :: outputPoEntries.toList

      // Create the new Po file containing the new and recovered strings.
      val newPoFile = IO.createTemporaryFile
      val freshPoFileWriter = new PoFileWriter(newPoFile, fileHeader.nPlural, finalEntries, Nil)
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
      IO.scanDirectory(runConfig.inputDirectory, _.isExtension("scala")) { f =>
        new ScalaFileReader(f, b).parse
      }
      makeScalaI18nCallSummaries(b.toList)
    }

    runConfig.masterLocalizations.foreach(mergeLocalization(_, i18nCallsInScalaFiles))
  }
}

