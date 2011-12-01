package net.strong_links.epoxy

import net.strong_links.core._
import java.io.File
import net.strong_links.i18ngen.I18ngen

object SbtI18ngenRunner {
  def apply(logger: Logger, localizations: List[I18nLocalization], packageName: String, inputDirectory: File, outputDirectory: File, searchNewI18n: Boolean) = {
    I18ngen.run(new Xlogger(logger), localizations, packageName, inputDirectory, outputDirectory)
  }
}

object SbtI18ngenConfiguration {

  def apply(logger: Logger, packageName: String, localizations: List[I18nLocalization], codeLocalization: I18nCodeLocalization, outputRootDirectory: File) = {
    //I18ngen.config(new Xlogger(logger), packageName, localizations, codeLocalization, outputDirectory)
  }
}