package net.strong_links.i18ngen

import net.strong_links.core._

object LanguageDefinition {
  def makeList(p: String) = {
    def clean(language: String, country: String, ctx: LoggingParameter) = {
      val l = language.trim.toLowerCase
      val c = country.trim.toUpperCase
      if (l.length != 2)
        Errors.fatal(ctx, "Invalid language code _." << l)
      if (!(c.isEmpty || (c.length == 2)))
        Errors.fatal(ctx, "Invalid country code _." << c)
      (l, c)
    }
    val list =
      (if (p.contains(",")) p.split(',').toList else List(p)).map(_.trim).filter(!_.isEmpty).map { s =>
        def ctx = "Invalid language/variant specification _" << s
        if (s.contains('_')) {
          val segments = s.split('_')
          if (segments.length != 2)
            Errors.fatal(ctx, "Expected 2 segments, found _." << segments.length)
          clean(segments(0), segments(1), ctx)
        } else
          clean(s.trim.toUpperCase, "", ctx)
      }
    val results = list.map(e => I18nLocale(e._1, e._2))
    results.foreach(loc => println(loc.getDisplayName))
    results.groupBy(I18nUtil.makeLanguageKey(_)).filter(_._2.length > 1).map(_._1) match {
      case Nil =>
      case list => Errors.fatal("Language/country _ appears more than once." << list.head)
    }
    results
  }
}