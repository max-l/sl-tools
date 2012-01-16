package net.strong_links.i18ngen

import net.strong_links.core._

object PoEntry {
  def formatIds(msgid: String, msgidPlural: Option[String]): String = {
    msgidPlural match {
      case None => "(_)" << msgid
      case Some(p) => "(_, _)" << (msgid, p)
    }
  }
}

//class MergeablePoEntry(val msgCtxt: Option[String], val msgid: String, val msgidPlural: Option[String], sequence: Int) {
//
//  var fuzzy = false
//  val key = msgCtxt match {
//    case None => msgid
//    case Some(c) => msgid + c
//  }
//  val sortKey = key.toLowerCase.filter(_.isLetterOrDigit).mkString + sequence.formatted("%09d")
//  val comments = scala.collection.mutable.ListBuffer[PoComment]()
//  private val references = scala.collection.mutable.ListBuffer[I18nReference]()
//  private var translations: Option[List[String]] = None
//
//  def hasSomeTranslations = {
//    translations match {
//      case None => false
//      case Some(list) => list.exists(!_.isEmpty)
//    }
//  }
//
//  private def check(what: String, s1: Any, s2: Any) {
//    if (s1 != s2)
//      Errors.fatal("Mismatch on _; _ vs. _." << (what, s1, s2))
//  }
//
//  def merge(poEntry: Po18nEntry) {
//    check("msgid", msgid, poEntry.msgid)
//    check("msgidPlural", msgidPlural, poEntry.msgidPlural)
//    comments ++= poEntry.comments
//    translations = Some(poEntry.translations)
//    fuzzy ||= poEntry.fuzzy
//  }
//
//  def fuzzyMerge(poEntry: Po18nEntry) {
//    translations = Some(poEntry.translations)
//    fuzzy = true
//  }
//
//  def makeEmptyTranslations(n: Int) = {
//    val b = new scala.collection.mutable.ListBuffer[String]()
//    for (i <- 0 until n)
//      b += ""
//    b.toList
//  }
//
//  def toPoEntry(nbPluralForms: Int): Po18nEntry = {
//    val k = if (msgidPlural == None) 1 else nbPluralForms
//    val t = translations match {
//      case None => makeEmptyTranslations(k)
//      case Some(list) =>
//        if (list.length != k)
//          Errors.fatal("Number of translations mismatch for msgid _; got _ while expecting _." << (msgid, list.length, k))
//        list
//    }
//    def cmp(a: I18nReference, b: I18nReference) =
//      (a.path compare b.path) match { case 0 => a.lineNumber compare b.lineNumber; case r => r }
//    new Po18nEntry(new I18nKey(msgCtxt, msgid, msgidPlural), comments.toList, references, t, fuzzy)
//  }
//}
//
//class PoEntryBag extends Logging {
//  private val h = scala.collection.mutable.HashMap[String, MergeablePoEntry]()
//  private var sequence = 0
//
//  init
//
//  def init {
//    println("RAPPEL: Laisser l'entrée vide dans le PO, c'est le header !!")
//  }
//
//  def getUntranslatedEntries = {
//    h.values.filter(!_.hasSomeTranslations)
//  }
//
//  def makeKey(msgCtxt: Option[String], msgid: String) = {
//    msgCtxt match {
//      case None => msgid
//      case Some(c) => msgCtxt + "\u0000" + msgid
//    }
//  }
//
//  def get(msgCtxt: Option[String], msgid: String) = {
//    h.get(makeKey(msgCtxt, msgid))
//  }
//
//  def merge(i18nCall: ScalaI18nCall) {
//    //    val key = makeKey(i18nCall.msgCtxt, i18nCall.msgid)
//    //    val x = h.get(key) match {
//    //      case None =>
//    //        sequence += 1
//    //        val e = new MergeablePoEntry(i18nCall.msgCtxt, i18nCall.msgid, i18nCall.msgidPlural, sequence)
//    //        h(key) = e
//    //        e
//    //      case Some(e) =>
//    //        e
//    //    }
//    //    if (x.msgidPlural == i18nCall.msgidPlural)
//    //      x.merge(i18nCall)
//    //    else {
//    //      val ids1 = PoEntry.formatIds(i18nCall.msgid, i18nCall.msgidPlural)
//    //      val ids2 = PoEntry.formatIds(x.msgid, x.msgidPlural)
//    //      logWarn(i18nCall.loggingReference,
//    //        "The string _ was ignored as it is incompatible with an other string _ previously defined."
//    //          <<< (ids1, ids2))
//    //    }
//  }
//
//  def getFinalPoEntries(nbPluralForms: Int) = {
//    h.values.toList.sortWith(_.sortKey < _.sortKey).map(_.toPoEntry(nbPluralForms))
//  }
//}