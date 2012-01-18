package net.strong_links.i18ngen

import net.strong_links.core._

class I18nGen(runConfig: RunConfig) extends Logging {

  def poFileFor(i18nLocalization: I18nLocalization) = i18nLocalization.fileFor(runConfig.inputDirectory, "po")

  def resourceFileFor(i18nLocalization: I18nLocalization) = i18nLocalization.fileFor(runConfig.outputDirectory, "scala")
}
