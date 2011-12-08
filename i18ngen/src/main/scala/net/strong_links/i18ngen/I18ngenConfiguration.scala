package net.strong_links.i18ngen

import net.strong_links.core._

import java.io.File

object I18ngenStandAloneConfiguration {

  import CommandLine._

  var stackTrace = false

  def execute(logger: Xlogger, args: Array[String]) {

    val programName = "I18ngenStandAloneConfiguration"
    var localizationsList: Option[String] = None
    var packageName: Option[String] = None
    var inputDirectory: Option[File] = None
    var outputDirectory: Option[File] = None
    var codeLanguage: Option[String] = None

    val config = List(
      Parameter("localizations", "List of localizations to generate.", ll => localizationsList = Some(ll)),
      Parameter("package name", "Name of the package (ex: com.company.xyz)", pn => packageName = Some(pn)),
      Parameter("package code language", "Localization of package code", pcl => codeLanguage = Some(pcl)),
      Parameter("input directory", "Root directory for the package.", id => inputDirectory = Some(new File(id))),
      Parameter("output directory", "Generated code root directory", od => outputDirectory = Some(new File(od))),
      SimpleSwitch("stackTrace", "Show stack trace upon error.", stackTrace = true),
      SimpleSwitch("debug", "Show debug log entries.", logger.enableDebug = true),
      Help("Examples of localizations: en,fr,en_uk:en,fr_ca:fr"))

    CommandLine(programName, config, args)

    (localizationsList, codeLanguage, packageName, inputDirectory, outputDirectory) match {
      case (Some(ll), Some(cl), Some(pn), Some(id), Some(od)) =>
        logger.info("_ started." << programName)
        val codeLocalization = I18nUtil.makeCodeLocalizationsFrom(cl)
        logger.debug("Package code localization: _" << codeLocalization)
        val localizations = I18nUtil.makeLocalizationsFrom(codeLocalization, ll)
        I18ngen.run(logger, localizations, pn, id, od)
        logger.info("_ ended." << programName)
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

