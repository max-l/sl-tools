package net.strong_links.i18ngen

import net.strong_links.core._

import java.io.File
import scala.collection.mutable.ListBuffer

import LexSymbol._

class PoFileReader(file: File, loggers: Loggers) extends PoReader(IO.loadUtf8TextFile(file), loggers) {
  override def getFileName = Some(file.getAbsolutePath)
}

class PoReader(data: String, loggers: Loggers) extends LexParser(data, loggers) {

  var emptyMsgidFound = false
  var savedStartLineNumber = 0
  
  val comments = new PoCommentBag
  val obsoleteComments = new CommentBag
  
  def isAutomatedComment(s: String) = 
    s.startsWith("#.") ||   // Comment extracted from Scala files 
    s.startsWith("#:") ||   // Source code reference
    s.startsWith("#,") ||   // Flag
    s.startsWith("#|") ||   // Previous msgid 
    s.startsWith("#~")      // Entries no longer used

  def processComment {
    if (symbolValue.startsWith("#, fuzzy"))
      comments.fuzzy = true
    if (!isAutomatedComment(symbolValue))
      comments.add(new TranslatorPoComment(symbolValue.substring(1)))
    if (symbolValue.startsWith("#~")) {
	  val sb = new StringBuilder
	  var done = false
	  while (!done) {
	    sb.append(symbolValue.substring(2).trim)
	    sb.append("\n")
	    done = !(currentChar == '#' && nextChar == '~')
	    super.getSymbol
	  }  
	  obsoleteComments.add(new ObsoletePoComment(sb.toString))
	} else
	  super.getSymbol
  }

  override def getSymbol {
    super.getSymbol
    while (symbol == poLineComments)
      processComment
  }
  
  def eatWhen(sym: LexSymbol) = {
    if (symbol == sym) {
      getSymbol
      Some(eatString)
    } else {
      None
    }
  }

  // Consider consecutive strings as a single string.
  override def eatString = {
    val s = super.eatString
    if (symbol == string) {
      val sb = new StringBuilder(s)
      do sb.append(super.eatString) while (symbol == string)
      sb.toString
    } else
      s
  }

  def recoverEntry: PoEntry = {
    val (fuzzy, accumulatedComments) = comments.obtainWithFuzzyAndClear
    savedStartLineNumber = startLineNumber
    val msgCtxt = eatWhen(msgctxt)
    skip(msgid)
    val msgidValue = eatString
    val msgidPlural = eatWhen(msgid_plural)
    var lastIndex = -1
    val translations = ListBuffer[String]()
    expect(msgstr)
    do {
      getSymbol
      val currentIndex =
        if (symbol == leftBracket) {
          getSymbol
          expect(number)
          val numberString = symbolValue
          getSymbol
          expect(rightBracket)
          getSymbol
          numberString.toInt
        } else {
          0
        }
      if (currentIndex != lastIndex + 1)
        error(savedStartLineNumber, "Expected index of translated message to be _, but got _." << (lastIndex + 1, currentIndex))
      lastIndex = currentIndex
      val t = eatString
      translations += t
    } while (symbol == msgstr)
    if (msgid == "") {
      if (emptyMsgidFound)
        error(savedStartLineNumber, "More than one empty 'msgid' found in the Po file.")
      emptyMsgidFound = true
    }
    new PoEntry(accumulatedComments, List[PoReference](), msgCtxt, msgidValue, msgidPlural, translations.toList, fuzzy)
  }

  def recoverEntriesWithSomeTranslations = {
    val entriesBuffer = ListBuffer[PoEntry]()
    var previousHeaderLine = 0
    var header: Option[PoEntry] = None
    getSymbol
    while (symbol != eof) {
      val e = recoverEntry
      if (e.hasSomeTranslations)
        entriesBuffer += e
      if (e.msgid == "") {
        if (header != None)
          error(savedStartLineNumber, "The Po file header entry (marked by a msgid \"\") appears twice, it first " +
                "appeared around line _." << previousHeaderLine)
          header = Some(e)
          previousHeaderLine = savedStartLineNumber
      }
    }
    (header, entriesBuffer.toList, obsoleteComments.obtainAndClear)
  }
}
