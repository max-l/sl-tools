package net.strong_links.i18ngen

import net.strong_links.core._

object PoFileHeader {

  def error(msg: String) = {
    Errors.fatal(("Invalid Po file header: _." << msg).format(true, false))    
  }

  def makeDefault(language: String, languageKey: String, packageName: String) = {
    var ok = true
    val pluralForm = language match {
      case "en" => "nplurals=2; plural=(n == 1) ? 0 : 1"
      case "fr" => "nplurals=2; plural=(n <= 1) ? 0 : 1"
      case _ => "nplurals=???; plural=???"; ok = false
    }
    val fileContents  =
      """|msgid ""
         |msgstr ""
         |"Project-Id-Version: _\n"
         |"PO-Revision-Date: YYYY-MM-DD HH:MI+ZONE\n"
         |"Language: _\n"
         |"Language-Team: _ <email@address>\n"
         |"Last-Translator: Full Name <email@address>\n"
         |"MIME-Version: 1.0\n"
         |"Content-Type: text/plain; charset=UTF-8\n"
         |"Content-Transfer-Encoding: 8bit\n"
         |"Plural-Forms: _\n"
         |""".stripMargin << (packageName, languageKey, languageKey, pluralForm)
    (ok, fileContents)
  }
}

import PoFileHeader._

class PoSplitter(s: String, splitWith: Char, subSplitWith: Char) {

  private val m = scala.collection.mutable.Map[String, String]()

  for (segment <- s.split(splitWith)) {
    val (segmentName, segmentValue) = splitAt(segment, subSplitWith, "segment _" <<< segment)
    if (m.contains(segmentName))
      PoFileHeader.error("duplicate _ found" <<< segmentName)
    m += (segmentName -> segmentValue)
  }

  def splitAt(s: String, at: Char, context: => String) = {
    def clean(x: String) = {
      val r = x.trim
      if (r == "")
        error("empty value found; _" <<< context)
      r
    }
    val pos = s.indexOf(at)
    if (pos == -1)
        error("separator _ not found; _" <<< (at, context))
    val name = s.substring(0, pos)
    val value = s.substring(pos + 1)
    (clean(name), clean(value))
  }

  def get(segmentName: String) = {
    m.get(segmentName) match {
      case None => error("missing _" <<< segmentName)
      case Some(value) => value
    }
  }
}

class PoFileHeader(entry: PoEntry, languageKey: String) {

  if (entry.msgid != "")
    error("'msgid' has a value _ while an empty string was expected" <<< entry.msgid)

  if (entry.translations.length != 1)
    error("_ translations found while one was expected" <<< entry.translations.length)

  val s = new PoSplitter(entry.translations.head.replace("\\n", "\uFFFF"), '\uFFFF', ':')
 
  private val foundLanguageKey = s.get("Language")
  if (foundLanguageKey != languageKey)  
    error("found language key _ while _ was expected" <<< (foundLanguageKey, languageKey))

  def getPluralInformation = {
    val ps = new PoSplitter(s.get("Plural-Forms"), ';', '=')
    val nPlurals = try ps.get("nplurals").toInt catch {
      case _ => error("invalid number _ found in the Plural-Forms segment" << ps.get("nplurals"))
    }
    (nPlurals, ps.get("plural"))
  }
}