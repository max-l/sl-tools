package net.strong_links.i18ngen

import net.strong_links.core._

import java.io.File
import java.util.Locale

object I18ngenStandAloneRunner {

  import CommandLine._

  var stackTrace = false

  def execute(logger: Xlogger, args: Array[String]) = {

    val programName = "I18ngenStandAloneRunner"
    var languages: Option[List[Locale]] = None
    var packageName: Option[String] = None
    var inputDirectory: Option[File] = None
    var outputDirectory: Option[File] = None
    var mergeEnabled = true

    val config = List(
      Parameter("languages", "List of languages to generate.", p => languages = Some(LanguageDefinition.makeList(p))),
      Parameter("package name", "Name of the package (ex: com.company.xyz)", pn => packageName = Some(pn)),
      Parameter("input directory", "Root directory for the package.", id => inputDirectory = Some(new File(id))),
      Parameter("output directory", "Generated code root directory", od => outputDirectory = Some(new File(od))),
      SimpleSwitch("stackTrace", "Show stack trace upon error.", stackTrace = true),
      SimpleSwitch("debug", "Show debug log entries.", logger.enableDebug = true),
      SimpleSwitch("nomerge", "Only generate resource classes from Po files", mergeEnabled = false))

    CommandLine(programName, config, args)

    def mergeForLocale(locale: Locale) = if (locale.getCountry == "") false else mergeEnabled

    (languages, packageName, inputDirectory, outputDirectory) match {
      case (Some(localeList), Some(pn), Some(id), Some(od)) =>
        logger.info("_ started." << programName)
        val filesCreated = new scala.collection.mutable.ListBuffer[File]
        localeList.foreach(locale => filesCreated += new I18nGenerator(logger).run(locale, pn, id, od, mergeForLocale(locale)))
        logger.info("_ ended." << programName)
        filesCreated.toList
      case _ =>
        Errors.fatal("Logic error; parameters can't be None at this point.")
    }
  }

  def main(args: Array[String]) {

    object logger extends StandAloneLogger

    try
      execute(new Xlogger(logger), args)
    catch {
      case e: Exception =>
        if (stackTrace)
          throw e
        else
          logger.error(e.getMessage)
    }
  }
}

