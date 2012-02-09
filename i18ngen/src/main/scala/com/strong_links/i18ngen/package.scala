package com.strong_links

package object i18ngen {

  implicit val scalaI18nCallSummaryComparer = new Ordering[SourceI18nCallSummary] {
    def compare(a: SourceI18nCallSummary, b: SourceI18nCallSummary): Int = a.key compare b.key
  }

  implicit val po18nEntryComparer = new Ordering[PoI18nEntry] {
    def compare(a: PoI18nEntry, b: PoI18nEntry): Int = a.key compare b.key
  }
}