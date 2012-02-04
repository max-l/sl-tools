package com.strong_links.i18ngen

import com.strong_links.core._
import com.strong_links.core.lex._

object PoHeaderInfo {

  def makeDefault(i18nConfig: I18nConfig, i18nLocale: I18nLocale): String = {

    def pluralRule = I18nKnownLocalization.getBest(i18nLocale.key) match {
      case None => "nplurals=???; plural=???"
      case Some(other) => other.poRule
    }

    """|msgid ""
         |msgstr ""
         |"Project-Id-Version: _1\n"
         |"PO-Revision-Date: _2\n"
         |"Language: _3\n"
         |"Language-Team: _3 <email@address>\n"
         |"Last-Translator: Full Name <email@address>\n"
         |"MIME-Version: 1.0\n"
         |"Content-Type: text/plain; charset=UTF-8\n"
         |"Content-Transfer-Encoding: 8bit\n"
         |"Plural-Forms: _4\n"
         |""".stripMargin << (i18nConfig.packageName, Util.nowAsStringWithTimeDelta, i18nLocale.key, pluralRule)
  }
}

class PoSplitter(s: String, splitWith: Char, subSplitWith: Char) {

  import PoHeaderInfo._

  private val m = scala.collection.mutable.Map[String, String]()

  for (segment <- Util.split(s, splitWith))
    Errors.trap("Segment _" << segment) {
      val (segmentName, segmentValue) = splitAt(segment, subSplitWith)
      if (m.contains(segmentName))
        Errors.fatal("Duplicate segment.")
      m(segmentName) = segmentValue
    }

  def splitAt(s: String, at: Char) = {
    def clean(x: String) = {
      val r = x.trim
      if (r == "")
        Errors.fatal("Empty value found.")
      r
    }
    val pos = s.indexOf(at)
    if (pos == -1)
      Errors.fatal("Separator _ not found." << at)
    val name = s.substring(0, pos)
    val value = s.substring(pos + 1)
    (clean(name), value.trim)
  }

  def get(segmentName: String) = {
    m.get(segmentName) match {
      case None => Errors.fatal("No segment named _ was found." << segmentName)
      case Some("") => Errors.fatal("Segment named _ is empty." << segmentName)
      case Some(value) => value
    }
  }
}

object PoPluralForm {
  def split(pluralForm: String) = {
    val ps = new PoSplitter(pluralForm, ';', '=')
    val nPlurals = try ps.get("nplurals").toInt catch {
      case e => Errors.fatal(e, "Invalid number _ found in the Plural-Forms segment." << ps.get("nplurals"))
    }
    (nPlurals, ps.get("plural"))
  }
}

class PoHeaderInfo(entry: PoI18nEntry, i18nLocale: I18nLocale) {

  val (nPlural, pluralForm) = Errors.trap("Invalid Po file header.") {
    if (entry.key.msgid != "")
      Errors.fatal("'msgid' has a value _ while an empty string was expected." << entry.key.msgid)
    entry.translations match {
      case List(singleTranslation) =>
        val splitter = new PoSplitter(LexParser.toRealLineFeeds(singleTranslation), '\n', ':')
        val languageKey = splitter.get("Language")
        if (languageKey != i18nLocale.key)
          Errors.fatal("Found language key _ while _ was expected." << (languageKey, i18nLocale.key))
        PoPluralForm.split(splitter.get("Plural-Forms"))
      case _ =>
        Errors.fatal("_ translations found while only one was expected." << entry.translations.length)
    }
  }
}