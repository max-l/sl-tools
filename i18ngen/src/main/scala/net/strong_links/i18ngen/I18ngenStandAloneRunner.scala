package net.strong_links.i18ngen

import net.strong_links.core._

import java.io.File

object I18ngenStandAloneRunner extends Logging {

  import CmdLine._

  def main(args: Array[String]) = {

    val DEFAULT_FUZZY_THRESHOLD = 0.3

    CmdLine(this, args, List(help("Example of localizations: fr,en_uk:en,fr_ca:fr"),
      help("Example of package code language: en"))).run(
      stringParameter("action", "Action name (merge, generate-resources, generate-catalog)"),
      stringParameter("package name", "Name of the package (ex: com.company.xyz)."),
      stringParameter("package code localization", "Localization of the package code."),
      stringParameter("localizations", "List of localizations to generate."),
      fileParameter("root source directory", "Root source directory."),
      fileParameter("root output directory", "Root output directory."),
      doubleSwitch("fuzzy-threshold", "ft", "Fuzzy match threshold override (default is _)" << DEFAULT_FUZZY_THRESHOLD)) {
        (action, packageName, languageKey, localizationsStr, inputDirectory, outputDirectory, optionalFuzzyThreshold) =>
          val fuzzyThreshold = optionalFuzzyThreshold.getOrElse(DEFAULT_FUZZY_THRESHOLD)
          if (fuzzyThreshold < 0)
            Errors.fatal("Fuzzy match threshold _ cannot be negative." << fuzzyThreshold)
          val runConfig = new RunConfig(packageName, languageKey, localizationsStr, fuzzyThreshold, inputDirectory, outputDirectory)
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
