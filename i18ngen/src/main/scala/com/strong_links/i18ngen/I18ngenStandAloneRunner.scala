package com.strong_links.i18ngen

import com.strong_links.core._
import java.io.File
import java.util.Locale

object I18ngenStandAloneRunner extends Logging {

  import CmdLine._

  def main(args: Array[String]) = {

    import java.util.Locale
    new I18nConfig2("a.b.c", I18nKnownLocalization.en_US, Seq(new Locale("fr")),
      Seq(new Locale("fr", "CA"), new Locale("fr", "BE"), new Locale("en", "UK"),
        new Locale("en", "UK", "Scottish"), new Locale("zg", "ZG")))
    OS.exitSuccess

    //    val fr = new I18nConfigLocalization(I18nLanguageKey.from("fr"))
    //    val en = new I18nConfigLocalization(I18nLanguageKey.from("en"))
    //    val fr_CA = new I18nConfigLocalization(I18nLanguageKey.from("fr_CA"), fr)
    //    val en_CA = new I18nConfigLocalization(I18nLanguageKey.from("fr_CA"), fr)
    //    val c = new I18nConfig2("a.b.c", I18nCodeLocalization.en(List("a", "b")), fr_CA, en_CA)

    // Specifications:
    // com.strong_links.scalaforms=en/fr,fr_CA:fr; com.strong_links.scalaforms.ui=es/en,fr,fr_CA:fr"

    Map(
      "com.strong_links.scalaforms" -> "en/fr,fr_CA:fr",
      "com.strong_links.scalaforms.ui" -> "es/en,fr,fr_CA:fr")

    CmdLine(this, args, List(help("Example of localizations: fr,en_uk:en,fr_ca:fr"),
      help("Example of package code language: en"))).run(
      stringParameter("action", "Action name (merge, generate-resources, generate-catalog)"),
      stringParameter("specifications", "Specifications of packages and localizations."),
      fileParameter("root source directory", "Root source directory."),
      fileParameter("root output directory", "Root output directory."),
      doubleSwitch("fuzzy-threshold", "ft", "Fuzzy match threshold override (default is _, disabled is 0)" << RunConfig.DEFAULT_FUZZY_THRESHOLD)) {
        (action, specifications, inputDirectory, outputDirectory, fuzzyThreshold) =>
          val runConfig = new RunConfig(specifications, fuzzyThreshold, inputDirectory, outputDirectory)
          action match {
            case "merge" =>
              I18nMerge.run(runConfig)
            case "generate-resources" =>
              I18nGenerateResources.run(runConfig)
            case "generate-catalog" =>
              I18nGenerateCatalog.run(runConfig)
            case other =>
              Errors.fatal("Invalid action _." << action)
          }
      }
  }
}
