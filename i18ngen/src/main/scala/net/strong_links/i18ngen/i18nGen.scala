package net.strong_links.i18ngen

import net.strong_links.core._
import java.util.Locale
import java.io.File
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

class I18nGen(runConfig: RunConfig) extends Logging {

  def makeScalaI18nCallSummaries(scalaI18nCalls: List[ScalaI18nCall]) = {
    def refs(calls: List[ScalaI18nCall]) = calls.map(_.reference).sorted
    I18nKey.group(scalaI18nCalls)(calls => "Referenced at _." << refs(calls)) {
      (msgCtxt, msgid, msgidPlural, calls) =>
        new ScalaI18nCallSummary(msgCtxt, msgid, msgidPlural, calls.flatMap(_.comments), refs(calls))
    }.sorted
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

  def fileFor(i18nLocalization: I18nLocalization, dir: File, extension: String) =
    new File(dir.path + IO.dirSeparator + i18nLocalization.className + extension)

  def poFileFor(i18nLocalization: I18nLocalization) = fileFor(i18nLocalization, runConfig.inputDirectory, ".po")

  def resourceFileFor(i18nLocalization: I18nLocalization) = fileFor(i18nLocalization, runConfig.outputDirectory, ".scala")

  def mergeLocalization(i18nLocalization: I18nLocalization, scalaI18nCallSummaries: List[ScalaI18nCallSummary]) {

    val poFile = poFileFor(i18nLocalization)

    if (!poFile.exists) {
      IO.writeUtf8ToFile(poFile, PoFileHeader.makeDefault(i18nLocalization))
      logInfo("_ created with default contents." << poFile)
    }

    val poFileReader = new PoFileReader(poFile, i18nLocalization)

    // Load the Po file and extract appropriate information from it.
    //val info = loadPoFile(poFile, i18nLocalization.languageKey)
    //
    //    // Get the final entries to be written to the resource file.
    //    merge(runConfig.inputDirectory, info.recoveredEntriesWithTranslations, info.nbPluralForms, poFile, info.obsoleteComments, scalaCallSummaries)
    //  }
    //
  }
}
