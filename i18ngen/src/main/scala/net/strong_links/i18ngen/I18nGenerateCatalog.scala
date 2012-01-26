package net.strong_links.i18ngen

import net.strong_links.core._
import java.io.File

object I18nGenerateCatalog extends Logging {

  def run(runConfig: RunConfig) = {

    def generate(i18nConfig: I18nConfig) = {

      val outputFile = runConfig.getOutputFileFor(i18nConfig.packageName, "PackageI18nConfig.scala")

      logDebug("Generating _." << outputFile)

      val b = scala.collection.mutable.ListBuffer[String]()
      b += ("package " + i18nConfig.packageName)
      b += ("")
      b += ("import net.strong_links.core._")
      b += ("")
      b += ("object PackageI18nConfig {")
      val x = i18nConfig.codeLocalization.i18nLanguageKey.string
      if (i18nConfig.allLocalizations == Nil) {
        b += ("  def catalog = new I18nConfig(\"_\", \"_\").toCatalog" << (i18nConfig.packageName, x))
      } else {
        val y = i18nConfig.allLocalizations.map(_.toString).mkString(",")
        b += ("  def catalog = new I18nConfig(\"_\", \"_\", \"_\").toCatalog" << (i18nConfig.packageName, x, y))
      }
      b += ("}")
      IO.writeUtf8ToFile(outputFile, b.mkString("\n"))
    }

    runConfig.i18nConfigs.foreach(generate)
  }
}

