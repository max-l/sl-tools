package net.strong_links.i18ngen

import net.strong_links.core._
import net.strong_links.core.lex._

object PoFileHeader {

  def makeDefault(i18nLocalization: I18nLocalization): String = {

    // Try to get the plural rule from the stock localizations we already know about.
    def pluralRule = try {
      I18nCodeLocalization(i18nLocalization.packageName, i18nLocalization.i18nLanguageKey).usePluralRulePoString
    } catch {
      case _ => "nplurals=???; plural=???"
    }

    import i18nLocalization._
    """|msgid ""
         |msgstr ""
         |"Project-Id-Version: _\n"
         |"PO-Revision-Date: _\n"
         |"Language: _\n"
         |"Language-Team: _ <email@address>\n"
         |"Last-Translator: Full Name <email@address>\n"
         |"MIME-Version: 1.0\n"
         |"Content-Type: text/plain; charset=UTF-8\n"
         |"Content-Transfer-Encoding: 8bit\n"
         |"Plural-Forms: _\n"
         |""".stripMargin << (packageName, Util.nowAsStringWithTimeDelta, i18nLanguageKey, i18nLanguageKey, pluralRule)
  }
}

class PoSplitter(s: String, splitWith: Char, subSplitWith: Char) {

  import PoFileHeader._

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
    (clean(name), clean(value))
  }

  def get(segmentName: String) = {
    m.get(segmentName) match {
      case None => Errors.fatal("No segment named _ was found." << segmentName)
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

class PoFileHeader(entry: Po18nEntry, i18nLocalization: I18nLocalization) {

  Errors.trap("Invalid Po file header.") {
    if (entry.msgid != "")
      Errors.fatal("'msgid' has a value _ while an empty string was expected." << entry.msgid)
    val (nPlural, pluralForm) = entry.translations match {
      case List(singleTranslation) =>
        val s = new PoSplitter(LexParser.toRealLineFeeds(singleTranslation), '\n', ':')
        if (s.get("Language") != i18nLocalization.i18nLanguageKey.string)
          Errors.fatal("Found language key _ while _ was expected." << (s.get("Language"), i18nLocalization.i18nLanguageKey))
        PoPluralForm.split(s.get("Plural-Forms"))
      case _ =>
        Errors.fatal("_ translations found while only one was expected." << entry.translations.length)
    }
  }
}