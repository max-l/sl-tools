package net.strong_links.i18ngen

import net.strong_links.core._
import net.strong_links.core.lex._

class PoI18nEntry(msgCtxt: Option[String], msgid: String, msgidPlural: Option[String], val comments: List[Comment], val translations: List[String], val references: List[I18nReference], val fuzzy: Boolean)
  extends Logging {

  val key = new I18nKey(msgCtxt, msgid, msgidPlural, true)

  if (msgid != "")
    translations.filter(!_.isEmpty).foreach(t => I18nKey.validate(Some(t), "msgstr"))

  def generate(nPlural: Int) = IO.usingCharStream { cs => new PoEntryWriter(cs, nPlural).write }

  class PoEntryWriter(cs: CharStream, nPlural: Int) {

    private def emitEmptyLine {
      cs.print("\n")
    }

    private def emitComments[T <: Comment](prefix: Char, comments: List[T]) {
      for (c <- comments)
        cs.print("#_ _\n" << (prefix, c.value))
    }

    private def emitComments {
      emitComments(' ', Util.filterOn[TranslatorPoComment](comments))
      emitComments('.', Util.filterOn[ScalaComment](comments))
    }

    private def emitReference(reference: I18nReference) {
      cs.println(reference.asPoComment)
    }

    private def getNextTranslation(it: Iterator[String]) = {
      if (it.hasNext)
        it.next
      else
        ""
    }

    private def niceString(s: String) = {
      val x = Util.split(LexParser.toRealLineFeeds(s), '\n')
      val segments = if (x.length == 1)
        List(s)
      else
        (x.init.map(_ + "\\n") :+ (if (x.last.isEmpty) Nil else x.last))
      segments.map("\"" + _ + "\"").mkString("\n")
    }

    private def emitNeutral(it: Iterator[String]) {
      cs.print("msgstr _\n" << niceString(getNextTranslation(it)))
    }

    private def emitNonNeutral(msgidPlural: String, it: Iterator[String]) {
      cs.print("msgid__plural _\n" << niceString(msgidPlural))
      for (i <- 0 until nPlural)
        cs.print("msgstr[_] _\n" << (i, niceString(getNextTranslation(it))))
    }

    def write {
      emitEmptyLine
      emitComments
      references.foreach(emitReference)
      if (fuzzy)
        cs.print("#, fuzzy\n")
      msgCtxt match {
        case None =>
        case Some(c) => cs.print("msgctxt _\n" << niceString(c))
      }
      cs.print("msgid _\n" << niceString(msgid))
      val it = translations.iterator
      msgidPlural match {
        case None => emitNeutral(it)
        case Some(p) => emitNonNeutral(p, it)
      }
      while (it.hasNext)
        logWarn("Discarded translation: _" << it.next)
    }
  }
}
