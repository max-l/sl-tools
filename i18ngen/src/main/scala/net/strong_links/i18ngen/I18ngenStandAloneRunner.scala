package net.strong_links.i18ngen

import net.strong_links.core._

import java.io.File

object I18ngenStandAloneRunner extends Logging {

  import CmdLine._

  def main(args: Array[String]) = {

    CmdLine(this, args, List(help("Examples of localizations: en,fr,en_uk:en,fr_ca:fr"))).run(
      intSwitch("drums", "nb of drums", "Number of drums owned."),
      stringParameter("localizations", "List of localizations to generate."),
      stringParameter("package name", "Name of the package (ex: com.company.xyz)"),
      stringParameter("package code language", "Localization of package code"),
      fileParameter("input directory", "Root directory for the package."),
      fileParameter("output directory", "Root directory for the generated files")) {
        (xxx, localizationsList, packageName, codeLanguage, inputDirectory, outputDirectory) =>
          val codeLocalization = I18nUtil.makeCodeLocalizationsFrom(codeLanguage)
          logDebug("Package code localization: _" << codeLocalization)
          val localizations = I18nUtil.makeLocalizationsFrom(codeLocalization, localizationsList)
          logDebug("Localizations: _" << localizations)
          I18ngen.run(localizations, packageName, inputDirectory, outputDirectory)
      }
  }
}
