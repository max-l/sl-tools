package com.strong_links.i18ngen

import com.strong_links.core._
import java.io.File

object I18nGenerateCatalogs extends Logging {

  def run(runConfig: RunConfig) = {

    def generate(i18nConfig: I18nConfig) = {

      val outputFile = runConfig.getOutputFileFor(i18nConfig.packageName, "I18nCatalog.scala")

      logInfo("Generating _." << outputFile)

      val b = scala.collection.mutable.ListBuffer[String]()
      b += ("package " + i18nConfig.packageName)
      b += ("")
      b += ("import com.strong_links.core._")
      b += ("")
      b += ("object i18nCatalog extends I18nCatalog(new I18nConfig(_))" << i18nConfig.serialize)
      IO.writeUtf8ToFile(outputFile, b.mkString("\n"))
      outputFile
    }

    runConfig.i18nConfigs.map(generate)
  }
}

