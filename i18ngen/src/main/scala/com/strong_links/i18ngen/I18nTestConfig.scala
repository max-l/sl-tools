package com.strong_links.i18ngen

import com.strong_links.core._
import java.io.File

object I18nTestConfig extends Logging {

  import Console._

  def run(runConfig: RunConfig): Seq[File] = {

    val configs = runConfig.i18nConfigs.sortWith(_.packageName < _.packageName)

    def testConfig(i18nConfig: I18nConfig, i18nLocale: I18nLocale) {
      println
      println("Localizations chain for user locale _ for package _" <<< (i18nLocale, i18nConfig.packageName))
      i18nConfig.getLocalizationsFmt(i18nLocale).foreach(loc => println(" -- _" << loc))
    }

    def process(userInput: String) {
      try {
        val i18nLocale = I18nLocale.from(userInput)
        configs.foreach(c => testConfig(c, i18nLocale))
      } catch {
        case t =>
          Console.err.println("Error: " + Errors.formatException(t, false))
      }
    }

    var done = false
    while (!done) {
      println
      print("Enter user input locale: ")
      val userInput = Console.readLine.trim
      done = userInput.isEmpty
      if (!done)
        process(userInput)
    }

    Nil
  }
}

