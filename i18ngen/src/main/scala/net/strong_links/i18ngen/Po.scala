package net.strong_links.i18ngen

import net.strong_links.core._

abstract class PoComment(val value: String) 

class ScalaPoComment(value: String) extends PoComment(value.trim)

class TranslatorPoComment(value: String) extends PoComment(value.trim)

class ObsoletePoComment(value: String) extends PoComment(value.trim)

class CommentBag {
  private val contents = scala.collection.mutable.ListBuffer[PoComment]()
   
  protected def clear {
    contents.clear
  }

  def add(comment: PoComment) {
    if (comment.value != "")
      contents += comment
  }

  def obtainAndClear: List[PoComment]  = {
    val list = contents.toList
    clear
    list
  }
}

class FlushableCommentBag(proximityThreshold: Int) extends CommentBag {

  private var lastLineNumber: Int = Int.MinValue

  override protected def clear {
    super.clear
    lastLineNumber = Int.MinValue
  }
  
  private def check(lineNumber: Int) {
    if ((lineNumber - lastLineNumber)  > proximityThreshold)
      clear
    lastLineNumber = lineNumber
  }

  def addAtLine(comment: PoComment, lineNumber: Int) {
    check(lineNumber)
    add(comment)
  }
  
  def obtainAtLine(lineNumber: Int): List[PoComment] = {
    check(lineNumber)
    obtainAndClear
  }
}

class PoCommentBag extends CommentBag {

  var fuzzy = false
  
  override protected def clear {
    super.clear
    fuzzy = false
  }
  
  def obtainWithFuzzyAndClear: (Boolean, List[PoComment])  = {
    (fuzzy, super.obtainAndClear)
  }
}

class PoReference(val absolutePath: String, val lineNumber: Int) {
  val sortAbsolutePath = absolutePath.toLowerCase
}

object PoReference {
  def sort(a: PoReference, b: PoReference) = {
    if (a.sortAbsolutePath < b.sortAbsolutePath) 
      true 
    else if (a.sortAbsolutePath > b.sortAbsolutePath) 
      false     
    else
      a.lineNumber < b.lineNumber
  }
}

class PoI18nCall(val comments: List[PoComment], val reference: Option[PoReference],
                 val msgCtxt: Option[String], val msgid: String, val msgidPlural: Option[String])

class PoEntry(val comments: List[PoComment], val references: List[PoReference],
              val msgCtxt: Option[String], val msgid: String, val msgidPlural: Option[String], 
              val translations: List[String], val fuzzy: Boolean) {
  
  override def toString: String = {
    IO.usingCharStream { cs => new PoEntryWriter(this, cs).write }
  }
  
  def hasSomeTranslations: Boolean = {
    translations.exists(!_.isEmpty)
  }

  def hasAllTranslations: Boolean = {
    translations.forall(!_.isEmpty)
  }
}

object PoEntry {
  def formatIds(msgid: String, msgidPlural: Option[String]): String = {
    msgidPlural match {
      case None => "(_)" << msgid 
      case Some(p) => "(_, _)" << (msgid, p)
    }
  }
}

class PoEntryWriter(entry: PoEntry, cs: CharStream) {

  private def emitEmptyLine {
    cs.write("\n")  
  }

  private def emitComments(prefix: Char, f: PoComment => Boolean) {
    for (c <- entry.comments.filter(f))
      cs.write("#_ _\n" << (prefix, c.value))
  }

  private def emitComments {
    emitComments(' ', _.isInstanceOf[TranslatorPoComment])
    emitComments('.', _.isInstanceOf[ScalaPoComment])
  }

  private def emitReference(reference: PoReference) {
    cs.write("#: _:_\n" << (reference.absolutePath, reference.lineNumber))
  }
  
  private def getNextTranslation(it: Iterator[String]) = {
    if (it.hasNext)
      it.next
    else
      ""
  }
  
  private def niceString(s: String) = {
    val x = s.replace("\\\\", "\uFFFF")
    if (x.contains("\\n"))
      "\"\n\"" + x.replace("\\n", "\\n\"\n\"").replace("\uFFFF", "\\")  
    else
      s
  }
  
  private def emitNeutral {
    val it = entry.translations.iterator
    cs.write("msgid \"_\"\n" << niceString(entry.msgid))
    cs.write("msgstr \"_\"\n" << niceString(getNextTranslation(it)))
  }

  private def emitNonNeutral {
    cs.write("msgid \"_\"\n" << niceString(entry.msgid))
    val Some(p) = entry.msgidPlural
    cs.write("msgid__plural \"_\"\n" << niceString(p))
    val it = entry.translations.iterator
    var i = 0
    while (it.hasNext) {
      cs.write("msgstr[_] \"_\"\n" << (i, niceString(getNextTranslation(it))))
      i += 1
    }
  }
  
  def write {
    emitEmptyLine
    emitComments
    for (r <- entry.references)
      emitReference(r)
    if (entry.fuzzy)
      cs.write("#, fuzzy\n")
    entry.msgCtxt match {
      case None =>
      case Some(c) => cs.write("msgctxt \"_\"\n" << niceString(c))
    }
    entry.msgidPlural match {
      case None => emitNeutral
      case Some(p) => emitNonNeutral
    }
  }
}

class MergeablePoEntry(val msgCtxt: Option[String], val msgid: String, val msgidPlural: Option[String], sequence: Int) {
  
  var fuzzy = false
  val key = msgCtxt match {
    case None => msgid
    case Some(c) => msgid + c
  }
  val sortKey = key.toLowerCase.filter(_.isLetterOrDigit).mkString + sequence.formatted("%09d")
  val comments =  scala.collection.mutable.ListBuffer[PoComment]()
  private val references = scala.collection.mutable.ListBuffer[PoReference]()
  private var translations: Option[List[String]] = None 
  
  def hasSomeTranslations = {
    translations match {
      case None => false
      case Some(list) => list.exists(!_.isEmpty) 
    }
  }
  
  private def check(what: String, s1: Any, s2: Any) {
    if (s1 != s2)
      Errors.fatal("Mismatch on _; _ vs. _." << (what, s1, s2))
  }
  
  def merge(i18nCall: PoI18nCall) {
    check("msgid", msgid, i18nCall.msgid)
    check("msgidPlural", msgidPlural, i18nCall.msgidPlural)
    comments ++= i18nCall.comments
    i18nCall.reference match {
      case Some(r) => references += r
      case None =>
    }
  }

  def merge(poEntry: PoEntry) {
    check("msgid", msgid, poEntry.msgid)
    check("msgidPlural", msgidPlural, poEntry.msgidPlural)
    comments ++= poEntry.comments
    translations = Some(poEntry.translations)
    fuzzy ||= poEntry.fuzzy
  }

  def fuzzyMerge(poEntry: PoEntry) {
    translations = Some(poEntry.translations)
    fuzzy = true
  }

  def makeEmptyTranslations(n: Int) = {
    val b = new scala.collection.mutable.ListBuffer[String]()
    for (i <- 0 until n)
      b += ""
    b.toList
  }
  
  def toPoEntry(nbPluralForms: Int): PoEntry = {
    val k = if (msgidPlural == None) 1 else nbPluralForms
    val t = translations match {
      case None => makeEmptyTranslations(k)
      case Some(list) => 
        if (list.length != k)
          Errors.fatal("Number of translations mismatch for msgid _; got _ while expecting _." << (msgid, list.length, k))
        list
    }
    new PoEntry(comments.toList, references.toList.sortWith(PoReference.sort), msgCtxt, msgid, msgidPlural, t, fuzzy)
  }
}

class PoEntryBag {
  private val h = scala.collection.mutable.HashMap[String, MergeablePoEntry]()
  private var sequence = 0

  init
  
  def init {
    val header = new PoI18nCall(List(new ScalaPoComment("PO file header.")), None, None, "", None)
    merge(header)
  }
  
  def getUntranslatedEntries = {
    h.values.filter(!_.hasSomeTranslations)
  }
  
  def makeKey(msgCtxt: Option[String], msgid: String) = {
    msgCtxt match {
      case None => msgid
      case Some(c) => msgCtxt + "\u0000" + msgid
    }
  }
  
  def get(msgCtxt: Option[String], msgid: String) = {
    h.get(makeKey(msgCtxt, msgid))
  }
  
  def merge(i18nCall: PoI18nCall) {
    val key = makeKey(i18nCall.msgCtxt, i18nCall.msgid)
    val x = h.get(key) match {
      case None => 
        sequence += 1
        val e = new MergeablePoEntry(i18nCall.msgCtxt, i18nCall.msgid, i18nCall.msgidPlural, sequence)
        h(key) = e
        e
      case Some(e) => 
        e
    }
    if (x.msgidPlural == i18nCall.msgidPlural)    
      x.merge(i18nCall)
    else {
      val ids1 = PoEntry.formatIds(i18nCall.msgid, i18nCall.msgidPlural)
      val ids2 = PoEntry.formatIds(x.msgid, x.msgidPlural)
      I18nGen.warning(i18nCall.reference, 
                      "The string _ was ignored as it is incompatible with an other string _ previously defined."  
                       <<< (ids1, ids2))
    }
  }  
  
  def getFinalPoEntries(nbPluralForms: Int) = {
    h.values.toList.sortWith(_.sortKey < _.sortKey).map(_.toPoEntry(nbPluralForms))
  }
}