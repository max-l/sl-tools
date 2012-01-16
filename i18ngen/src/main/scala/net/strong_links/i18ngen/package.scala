package net.strong_links

package object i18ngen {

  implicit val scalaI18nCallSummaryComparer = new Ordering[ScalaI18nCallSummary] {
    def compare(a: ScalaI18nCallSummary, b: ScalaI18nCallSummary): Int = (a: I18nKey) compare (b: I18nKey)
  }

  implicit val po18nEntryComparer = new Ordering[Po18nEntry] {
    def compare(a: Po18nEntry, b: Po18nEntry): Int = (a: I18nKey) compare (b: I18nKey)
  }
}