package com.strong_links.i18ngen

import com.strong_links.core._
import java.io.File

object I18nGenerateCatalog extends Logging {

  def run(runConfig: RunConfig) = {

    def generate(i18nConfig: I18nConfig) = {

      val outputFile = runConfig.getOutputFileFor(i18nConfig.packageName, "PackageI18nConfig.scala")

      logDebug("Generating _." << outputFile)

      val b = scala.collection.mutable.ListBuffer[String]()
      b += ("package " + i18nConfig.packageName)
      b += ("")
      b += ("import com.strong_links.core._")
      b += ("")
      b += ("object PackageI18nConfig {")
      b += ("  def catalog = new I18nConfig(_).toCatalog" << i18nConfig.serialize)
      b += ("}")
      IO.writeUtf8ToFile(outputFile, b.mkString("\n"))
      outputFile
    }

    runConfig.i18nConfigs.map(generate)
  }
}

