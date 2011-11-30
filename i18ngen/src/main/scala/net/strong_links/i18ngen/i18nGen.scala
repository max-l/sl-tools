package net.strong_links.i18ngen

import net.strong_links.core._
import java.io.File
import java.util.Locale
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

class I18nCatalog(val packageName: String, val sourceRoot: String, val languageCode: String, val countryCode: String = "")

class I18nGenerator(logger: Xlogger) {

  // Constants.
  val PROGRAM_NAME = "i18nGen"
  val DUMMY_PO_ENTRY = new PoEntry(Nil, Nil, None, "", None, Nil, false)

  // Program options with default values.
  var mergeEnabled = true
  var logEnabled = false
  var country = ""

  def log(msg: => String) {
    if (logEnabled)
      println("  - Log: _" << msg)
  }

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

  def getNames(locale: Locale, packageRoot: String, packageName: String, outputDirectory: String) = {
    val languageKey = I18nUtil.makeLanguageKey(locale)
    val className = I18nUtil.classNameFor(packageName, languageKey)
    val packageDirectoryName = packageRoot + IO.dirSeparator + packageName.replace(".", IO.dirSeparator)
    val poFileName = packageDirectoryName + IO.dirSeparator + className + ".po"
    val resourceFileName = outputDirectory + IO.dirSeparator + className + ".scala"
    (languageKey, packageDirectoryName, poFileName, className, resourceFileName)
  }

  // Extract whatever valid PoEntries we can from obsolete Po Comments.
  def extractEntriesFrom(obsoleteComments: List[PoComment]) = {
    // Define a little class for a PoParser that will never crash upon failure. We
    // want to do that because the seeked Po entries are embedded within comments,
    // and by definition, comments can be any kind of junk, so we need to plan for
    // this.
    val oops = new Exception
    class TolerantParser(data: String) extends PoReader(data, logger) {
      override def error(startLineNumber: Int, msg: LoggingParameter) = throw oops
      def parse = try recoverEntriesWithSomeTranslations catch {
        case e if (e eq oops) => (None, Nil, Nil)
        case e => Errors.fatal("Unexpected parsing error: _." << e.getMessage)
      }
    }
    // And use it on every obsolete comment found in the PoFile.
    for (oc <- obsoleteComments; r <- new TolerantParser(oc.value).parse._2)
      yield r
  }

  def merge(packageDirectoryName: String, recoveredEntriesWithTranslations: List[PoEntry],
    nbPluralForms: Int, poFile: File, obsoleteComments: List[PoComment]) = {

    // Create a container for the Po entries that will be merged.
    val mergedEntries = new PoEntryBag(logger)

    // Scan the Scala source files, extracting the mergeable I18n entries. These entries here are
    // "minimalist", as they do not have any translations yet.
    println("  - Scanning source Scala files.")
    val isScalaFile = (f: File) => f.getAbsolutePath.toLowerCase.endsWith(".scala")
    IO.scanDirectory(new File(packageDirectoryName), isScalaFile)(new ScalaFileReader(_, mergedEntries, logger).scan)

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
    log("Deleting _" <<< poFile.getAbsolutePath)
    IO.deleteFile(poFile)
    log("Renaming _ to _" <<< (freshPoFile.getAbsolutePath, poFile.getAbsolutePath))
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

    val poFileReader = new PoFileReader(poFile, logger)

    // Get the entries in the original PO file, along with obsolete comments. 
    val (possibleHeaderEntry, recoveredEntriesWithTranslations, obsoleteComments) =
      poFileReader.recoverEntriesWithSomeTranslations

    // Make sure we have a file header
    val headerEntry = possibleHeaderEntry match {
      case None => Errors.fatal("Missing Po file header.")
      case Some(e) => e
    }

    // Extract some run-time parameters from the Po file header. 
    val poHeader = new PoFileHeader(headerEntry, languageKey)
    val (nbPluralForms, pluralForms) = poHeader.getPluralInformation
    log("Nb plural forms: _" << nbPluralForms)
    log("Plural forms: _" << pluralForms)

    (recoveredEntriesWithTranslations, obsoleteComments, nbPluralForms, pluralForms)
  }

  def getArguments(args: Array[String]) = {
    var language, packageRoot, packageName, outputDirectory = ""
    import CommandLine._
    val config = List(
      ValuedSwitch("country", "code", "Two-letter country code (US, FR, CA, ...)", country = _),
      SimpleSwitch("nomerge", "Only generate a resource class from the Po file", mergeEnabled = false),
      SimpleSwitch("log", "Display additional logging information", logEnabled = true),
      Parameter("language", "Two-letter language code (en, fr, de, ...)", language = _),
      Parameter("package root", "Path where the package resides (ex: /home/joe/prj)", packageRoot = _),
      Parameter("package name", "Name of the package (ex: com.company.xyz)", packageName = _),
      Parameter("output directory", "Path of the directory where the Scala resource file will be created", outputDirectory = _),
      Help("A country-neutral resource file is generated when no country code is specified."))
    new CommandLine(PROGRAM_NAME, config).run(args)
    (language, country, packageRoot, packageName, outputDirectory)
  }

  def main(args: Array[String]) {

    // Get the command line arguments
    val (language, country, packageRoot, packageName, outputDirectory) = getArguments(args)

    run(language, country, packageRoot, packageName, outputDirectory)
  }

  def run(language: String, country: String, packageRoot: String, packageName: String, outputDirectory: String): File = {
    // Create the target locale
    val targetLocale = I18nLocale(language, country)

    // Construct the names of files, etc. we need.
    val (languageKey, packageDirectoryName, poFileName, className, resourceFileName) =
      getNames(targetLocale, packageRoot, packageName, outputDirectory)

    println("Running _ on _." <<< (PROGRAM_NAME, poFileName))
    println("  - Target language key: _." <<< I18nUtil.makeLanguageKey(targetLocale))

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
        if (mergeEnabled)
          merge(packageDirectoryName, recoveredEntriesWithTranslations, nbPluralForms, poFile, obsoleteComments)
        else
          recoveredEntriesWithTranslations
      }.filter(e => !e.fuzzy && e.hasAllTranslations)

      // Generate the resource file
      val scalaResourceFile = new File(resourceFileName)
      val resourceFileWriter = new ResourceFileWriter(scalaResourceFile, className, languageKey, nbPluralForms,
        pluralForms, finalEntries, logger)
      resourceFileWriter.generateAndClose
      println("  - File _ generated successfully." <<< resourceFileName)
      scalaResourceFile
    }
  }
}
