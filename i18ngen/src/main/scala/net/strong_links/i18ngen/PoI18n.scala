package net.strong_links.i18ngen

import net.strong_links.core._
import net.strong_links.core.lex._

object PoI18nEntry {

  def toMap(list: List[PoI18nEntry]) = list.map(e => (e.key, e)).toMap

  def makeFrom(scs: ScalaI18nCallSummary) =
    new PoI18nEntry(scs.key.msgCtxt, scs.key.msgid, scs.key.msgidPlural, scs.comments, Nil, scs.references, false)

  def makeFuzzyFrom(po: PoI18nEntry) =
    new PoI18nEntry(po.key.msgCtxt, po.key.msgid, po.key.msgidPlural, po.comments, po.translations, Nil, true)

  def makeFuzzyFrom(po: PoI18nEntry, scs: ScalaI18nCallSummary) =
    new PoI18nEntry(scs.key.msgCtxt, scs.key.msgid, scs.key.msgidPlural, scs.comments, po.translations, scs.references, true)

  def merge(scs: ScalaI18nCallSummary, po: PoI18nEntry) = {
    if (scs.key != po.key)
      Errors.fatal("Attempted to merge !_ with !_." << (scs.key, po.key))
    if (scs.key.msgidPlural != po.key.msgidPlural)
      Errors.fatal("Entries !_ and !_ have incompatible msgid_plural values _ and _." <<
        (scs.key, po.key, scs.key.msgidPlural, po.key.msgidPlural),
        "Referenced at !_." << scs.references)
    new PoI18nEntry(po.key.msgCtxt, po.key.msgid, po.key.msgidPlural, po.comments ::: scs.comments, po.translations, scs.references, po.fuzzy)
  }
}

class PoI18nEntry(msgCtxt: Option[String], msgid: String, msgidPlural: Option[String], val comments: List[Comment], val translations: List[String], val references: List[I18nReference], val fuzzy: Boolean)
  extends Logging {

  val key = new I18nKey(msgCtxt, msgid, msgidPlural, true)

  override def toString = key.toString

  if (msgid != "")
    translations.filter(!_.isEmpty).foreach(t => I18nUtil.validate(Some(t), "msgstr"))

  private def okStatus = "OK"

  def translationStatusIsOK(nPlural: Int) = translationStatus(nPlural) == okStatus

  def translationStatus(nPlural: Int) = {
    val nRequiredTranslations = if (msgidPlural == None) 1 else nPlural
    def msg(t: String, n: Int): String =
      Util.sp("__ _ translation for __." << t, "__ _ translations for __." << t, n) << (n, key)
    if (msgid == "")
      okStatus
    else {
      val delta = translations.length - nRequiredTranslations
      if (delta < 0)
        msg("missing", -delta)
      else if (delta > 0)
        msg("extraneous", delta)
      else {
        val n = translations.count(_.isEmpty)
        if (n > 0)
          msg("empty", n)
        else
          okStatus
      }
    }
  }

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
        logWarn("Discarded translation _ for !_." << (it.next, key))
    }
  }
}

