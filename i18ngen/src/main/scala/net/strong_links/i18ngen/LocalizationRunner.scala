package net.strong_links.i18ngen

import net.strong_links.core._

class LocalizationRunner(i18nLocalization: I18nLocalization) extends LoggingPrefixed {
  protected val loggingPrefixSeq: Seq[LoggingParameter] = Seq("Localization _" << i18nLocalization.i18nLanguageKey.string)
}