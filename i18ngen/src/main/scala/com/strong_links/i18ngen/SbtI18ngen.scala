package com.strong_links.epoxy

import com.strong_links.core._
import java.io.File

object SbtI18ngenRunner {
  def apply(localizations: List[I18nLocalization], packageName: String, inputDirectory: File, outputDirectory: File, searchNewI18n: Boolean) = {
    // I18ngen.run(localizations, packageName, inputDirectory, outputDirectory)
  }
}

object SbtI18ngenConfiguration {

  def apply(packageName: String, localizations: List[I18nLocalization], codeLocalization: I18nCodeLocalization, outputRootDirectory: File) = {
    //I18ngen.config(new Xlogger(logger), packageName, localizations, codeLocalization, outputDirectory)
  }
}