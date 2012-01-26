package net.strong_links.i18ngen

import net.strong_links.core._

import java.io.File

object I18ngenStandAloneRunner extends Logging {

  import CmdLine._

  def main(args: Array[String]) = {

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
