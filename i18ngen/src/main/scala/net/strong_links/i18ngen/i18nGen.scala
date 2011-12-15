package net.strong_links.i18ngen

import net.strong_links.core._
import java.io.File
import java.util.Locale
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

class I18nCatalog(val packageName: String, val sourceRoot: String, val languageCode: String, val countryCode: String = "")

class I18nGenerator extends Logging {

  // Constants.
  val PROGRAM_NAME = "i18nGen"
  val DUMMY_PO_ENTRY = new PoEntry(Nil, Nil, None, "", None, Nil, false)

  def tryExactMatch(e: PoEntry, entryBag: PoEntryBag): Boolean = {
    entryBag.get(e.msgCtxt, e.msgid) match {
      case Some(x) if (x.msgidPlural == e.msgidPlural) =>
        x.merge(e)
        true
      case _ =>
        false
    }
  }

  def exactMatch(recoveredEntriesWithTranslations: List[PoEntry], mergedEntries: PoEntryBag) = {
    val backupComments = new CommentBag
    for (e <- recoveredEntriesWithTranslations)
      if (!tryExactMatch(e, mergedEntries))
        if (!e.fuzzy && e.hasSomeTranslations)
          backupComments.add(new ObsoletePoComment(e.toString))
    backupComments.obtainAndClear
  }

  def bestFuzzyMatch(msgid: String, recoveredEntriesWithTranslations: List[PoEntry]): (Int, PoEntry) = {
    ((Int.MaxValue, DUMMY_PO_ENTRY) /: recoveredEntriesWithTranslations) { (a, b) =>
      val cost = Util.getLevenshteinDistance(b.msgid, msgid)
      if (cost < a._1) (cost, b) else a
    }
  }

  def fuzzyMatch(matchablEntries: List[PoEntry], untranslatedEntries: Iterable[MergeablePoEntry]) {
    // Threshold was determined experimentally.
    val WEIGHTED_COST_THRESHOLD = 0.3
    val r = matchablEntries
    for (e <- untranslatedEntries) {
      val (cost, bestMatch) = bestFuzzyMatch(e.msgid, r)
      val weightedCost = cost.toDouble / (e.msgid.length + bestMatch.msgid.length)
      val retain = weightedCost < WEIGHTED_COST_THRESHOLD
      if (retain) {
        println("  - Fuzzy: _ translations used for _." <<< (bestMatch.msgid, e.msgid))
        e.fuzzyMerge(bestMatch)
      }
    }
  }

  def getNames(locale: Locale, packageName: String, inputDirectory: File, outputDirectory: File) = {
    val languageKey = I18nUtil.makeLanguageKey(locale)
    val className = I18nUtil.classNameFor(packageName, languageKey)
    val poFileName = inputDirectory.getCanonicalPath + IO.dirSeparator + className + ".po"
    val resourceFileName = outputDirectory + IO.dirSeparator + className + ".scala"
    (languageKey, poFileName, className, resourceFileName)
  }

  // Extract whatever valid PoEntries we can from obsolete Po Comments.
  def extractEntriesFrom(obsoleteComments: List[PoComment]) = {
    // Define a little class for a PoParser that will never crash upon failure. We
    // want to do that because the seeked Po entries are embedded within comments,
    // and by definition, comments can be any kind of junk, so we need to plan for
    // this.
    class TolerantPoReader(data: String) extends PoReader(data) {
      override def parse = try super.parse catch {
        case _ => (None, Nil, Nil)
      }
    }
    // And use it on every obsolete comment found in the PoFile.
    for (oc <- obsoleteComments; r <- new TolerantPoReader(oc.value).parse._2)
      yield r
  }

  def merge(inputDirectory: File, recoveredEntriesWithTranslations: List[PoEntry],
    nbPluralForms: Int, poFile: File, obsoleteComments: List[PoComment]) = {

    // Create a container for the Po entries that will be merged.
    val mergedEntries = new PoEntryBag

    // Scan the Scala source files, extracting the mergeable I18n entries. These entries here are
    // "minimalist", as they do not have any translations yet.
    val isScalaFile = (f: File) => f.getAbsolutePath.toLowerCase.endsWith(".scala")
    IO.scanDirectory(inputDirectory, isScalaFile)(new ScalaFileReader(_, mergedEntries).scan)

    // Try exact matches between recovered entries and new entries. The method returns the entries 
    // that did not macthed as obsolete comments.
    val moreObsoleteComments = exactMatch(recoveredEntriesWithTranslations, mergedEntries)

    // Parse the obsolete entries comments as much as we can to get entries on which fuzzy matches
    // will also be done.
    val entriesFromObsoleteComments = extractEntriesFrom(obsoleteComments ::: moreObsoleteComments)

    // Try fuzzy matches between recovered entries (non-fuzzy ones only) and new entries that 
    // still do not have translations.
    val matchableEntries =
      (recoveredEntriesWithTranslations ::: entriesFromObsoleteComments).filter(e => !(e.msgid == "" || e.fuzzy))
    fuzzyMatch(matchableEntries, mergedEntries.getUntranslatedEntries)

    // Create the new Po file containing the new and recovered strings.
    val newPoEntries = mergedEntries.getFinalPoEntries(nbPluralForms)
    val freshPoFile = IO.createTemporaryFile
    val freshPoFileWriter = new PoFileWriter(freshPoFile, nbPluralForms, newPoEntries, obsoleteComments)
    freshPoFileWriter.generateAndClose

    // The newly merged Po file can now replace the old Po file.
    logDebug("Deleting _" <<< poFile.getAbsolutePath)
    IO.deleteFile(poFile)
    logDebug("Renaming _ to _" <<< (freshPoFile.getAbsolutePath, poFile.getAbsolutePath))
    IO.renameFile(freshPoFile, poFile)

    newPoEntries
  }

  def makePoFile(poFileName: String, packageName: String, locale: Locale, languageKey: String) = {
    val f = new File(poFileName)
    var created = false
    if (!f.exists) {
      val (complete, contents) = PoFileHeader.makeDefault(locale.getLanguage, languageKey, packageName)
      IO.writeUtf8ToFile(f, contents)
      if (!complete) {
        println("A new Po file was created for you, but it is still incomplete.")
        println("Please fix it manually and try again. The new Po file is located at")
        println("following location:")
        println(" -- > _" << f.getAbsolutePath)
        created = true
      }
    }
    (created, f)
  }

  def loadPoFile(poFile: File, languageKey: String) = {

    val poFileReader = new PoFileReader(poFile)

    // Get the entries in the original PO file, along with obsolete comments. 
    val (possibleHeaderEntry, recoveredEntriesWithTranslations, obsoleteComments) =
      poFileReader.parse

    // Make sure we have a file header
    val headerEntry = possibleHeaderEntry match {
      case None => Errors.fatal("Missing Po file header.")
      case Some(e) => e
    }

    // Extract some run-time parameters from the Po file header. 
    val poHeader = new PoFileHeader(headerEntry, languageKey)
    val (nbPluralForms, pluralForms) = poHeader.getPluralInformation
    logDebug("Nb plural forms: _" << nbPluralForms)
    logDebug("Plural forms: _" << pluralForms)

    (recoveredEntriesWithTranslations, obsoleteComments, nbPluralForms, pluralForms)
  }

  def run(targetLocale: Locale, packageName: String, inputDirectory: File, outputDirectory: File, pSearchNewI18n: Boolean): File = {

    // Ensure that we never search for new I18n strings when dealing with specific countries, this would be OTT.
    val searchNewI18n = if (targetLocale.getCountry == "") false else pSearchNewI18n

    // Ensure input and output directories exist.
    IO.checkForExistingDirectory(inputDirectory)
    IO.createDirectory(outputDirectory, true)

    // Construct the names of files, etc. we need.
    val (languageKey, poFileName, className, resourceFileName) =
      getNames(targetLocale, packageName, inputDirectory, outputDirectory)

    logDebug("Po file: _" <<< poFileName)
    logDebug("Target locale: _." <<< I18nUtil.makeLanguageKey(targetLocale))

    // A new PO file is created with default contents if it does not exist.
    val (poFileCreated, poFile) = makePoFile(poFileName, packageName, targetLocale, languageKey)

    if (poFileCreated) {
      val f = new File(resourceFileName) // in this case File(resourceFileName) already exists ?
      assert(f.exists())
      f
    } else {
      // Load the Po file and extract appropriate information from it.
      val (recoveredEntriesWithTranslations, obsoleteComments, nbPluralForms, pluralForms) =
        loadPoFile(poFile, languageKey)

      // Get the final entries to be written to the resource file.
      val finalEntries = {
        if (searchNewI18n)
          merge(inputDirectory, recoveredEntriesWithTranslations, nbPluralForms, poFile, obsoleteComments)
        else
          recoveredEntriesWithTranslations
      }.filter(e => !e.fuzzy && e.hasAllTranslations)

      // Generate the resource file
      val scalaResourceFile = new File(resourceFileName)
      val resourceFileWriter = new ResourceFileWriter(scalaResourceFile, className, languageKey, nbPluralForms,
        pluralForms, finalEntries)
      resourceFileWriter.generateAndClose
      logDebug("File _ generated successfully." <<< resourceFileName)
      scalaResourceFile
    }
  }
}

object I18ngen {

  def run(localizations: List[I18nLocalization], packageName: String, inputDirectory: File, outputDirectory: File) = {
    val filesCreated = new scala.collection.mutable.ListBuffer[File]
    localizations.foreach {
      val g = new I18nGenerator
      localization => filesCreated += g.run(localization.locale, packageName, inputDirectory, outputDirectory, localization.parent == None)
    }
    filesCreated.toList
  }
}
