package net.strong_links.i18ngen

import net.strong_links.core._
import net.strong_links.core.lex._

class Po18nEntry(msgCtxt: Option[String], msgid: String, msgidPlural: Option[String], val comments: List[Comment], val translations: List[String], val references: List[I18nReference], val fuzzy: Boolean)
  extends I18nKey(msgCtxt, msgid, msgidPlural, true) {

  translations.filter(!_.isEmpty).foreach(t => validate(Some(t), "msgstr"))

  override def toString: String = {
    IO.usingCharStream { cs => new PoEntryWriter(cs).write }
  }

  class PoEntryWriter(cs: CharStream) {

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
      val x = LexParser.toRealLineFeeds(s)
      if (x.contains("\n"))
        "\"\n\"" + x.replace("\n", "\"\n\"")
      else
        s
    }

    private def emitNeutral {
      val it = translations.iterator
      cs.print("msgstr \"_\"\n" << niceString(getNextTranslation(it)))
    }

    private def emitNonNeutral(msgidPlural: String) {
      cs.print("msgid__plural \"_\"\n" << niceString(msgidPlural))
      val it = translations.iterator
      var i = 0
      while (it.hasNext) {
        cs.print("msgstr[_] \"_\"\n" << (i, niceString(getNextTranslation(it))))
        i += 1
      }
    }

    def write {
      emitEmptyLine
      emitComments
      for (r <- references)
        emitReference(r)
      if (fuzzy)
        cs.print("#, fuzzy\n")
      msgCtxt match {
        case None =>
        case Some(c) => cs.print("msgctxt \"_\"\n" << niceString(c))
      }
      cs.print("msgid \"_\"\n" << niceString(msgid))
      msgidPlural match {
        case None => emitNeutral
        case Some(p) => emitNonNeutral(p)
      }
    }
  }
}

