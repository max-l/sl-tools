package net.strong_links

package object i18ngen {

  implicit val scalaI18nCallSummaryComparer = new Ordering[ScalaI18nCallSummary] {
    def compare(a: ScalaI18nCallSummary, b: ScalaI18nCallSummary): Int = a.key compare b.key
  }

  implicit val po18nEntryComparer = new Ordering[PoI18nEntry] {
    def compare(a: PoI18nEntry, b: PoI18nEntry): Int = a.key compare b.key
  }
}