package net.strong_links.i18ngen

import net.strong_links.core._

import java.io.File

object I18ngenStandAloneRunner extends Logging {

  import CmdLine._

  def main(args: Array[String]) = {

    CmdLine(this, args, List(help("Example of localizations: fr,en_uk:en,fr_ca:fr"),
      help("Example of package code language: en"))).run(
      stringParameter("action", "Action name (catalog, merge, generate)"),
      stringParameter("package name", "Name of the package (ex: com.company.xyz)."),
      stringParameter("package code localization", "Localization of the package code."),
      stringParameter("localizations", "List of localizations to generate."),
      fileParameter("root source directory", "Root source directory."),
      fileParameter("root output directory", "Root output directory.")) {
        (action, packageName, languageKey, localizationsStr, inputDirectory, outputDirectory) =>
          val runConfig = new RunConfig(packageName, languageKey, localizationsStr, inputDirectory, outputDirectory)
          action match {
            case "catalog" =>
              Errors.notImplemented
            case "merge" =>
              new I18nGenMerge(runConfig).merge
            case "generate" =>
              new I18nGenGenerate(runConfig).generate
            case other =>
              Errors.fatal("Invalid action _." << action)
          }
      }
  }
}
