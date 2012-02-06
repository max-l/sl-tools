package com.strong_links.i18ngen

import com.strong_links.core._
import java.io.File
import java.util.Locale

object I18ngenStandAloneRunner extends Logging {

  import CmdLine._

  def main(args: Array[String]) = {

    CmdLine(this, args, List(help("Example of localizations: fr,en_uk:en,fr_ca:fr"),
      help("Example of package code language: en"))).run(
      stringParameter("action", "Action name (scan-and-merge, generate-resources, generate-catalogs)"),
      stringParameter("specifications", "Specifications of packages and localizations."),
      fileParameter("root source directory", "Root source directory."),
      fileParameter("root output directory", "Root output directory."),
      doubleSwitch("fuzzy-threshold", "ft", "Fuzzy match threshold override (default is _, disabled is 0)" << RunConfig.DEFAULT_FUZZY_THRESHOLD)) {
        (action, specifications, inputDirectory, outputDirectory, fuzzyThreshold) =>
          val runConfig = new RunConfig(specifications, fuzzyThreshold, inputDirectory, outputDirectory)
          action match {
            case "scan-and-merge" =>
              I18nScanAndMerge.run(runConfig)
            case "generate-resources" =>
              I18nGenerateResources.run(runConfig)
            case "generate-catalogs" =>
              I18nGenerateCatalogs.run(runConfig)
            case other =>
              Errors.fatal("Invalid action _." << action)
          }
      }
  }
}
