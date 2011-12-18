package net.strong_links.i18ngen

import net.strong_links.core._
import net.strong_links.core.lex._

import java.io.File
import scala.collection.mutable.ListBuffer

class PoFileReader(file: File) extends PoReader(IO.loadUtf8TextFile(file)) {
  override def parse = Errors.liveTrap("PO _" << file) {
    super.parse
  }
}

class PoReader(data: String) extends LexParser(data) {

  val Msgid, Msgctxt, Msgid_plural, Msgstr = idSymbol
  val leftBracket = specialSymbol("[")
  val rightBracket = specialSymbol("]")
  val poLineComments = symbol

  var emptyMsgidFound = false
  val comments = new PoCommentBag
  val obsoleteComments = new CommentBag

  override def getMiscellaneous {
    if (currentChar == '"')
      getString
    else if (currentChar == '#' && previousChar == '\n')
      getLineComments(poLineComments)
    else
      super.getMiscellaneous
  }

  def isAutomatedComment(s: String) =
    s.startsWith("#.") || // Comment extracted from Scala files 
      s.startsWith("#:") || // Source code reference
      s.startsWith("#,") || // Flag
      s.startsWith("#|") || // Previous msgid 
      s.startsWith("#~") // Entries no longer used

  def processComment {
    if (token.value.startsWith("#, fuzzy"))
      comments.fuzzy = true
    if (!isAutomatedComment(token.value))
      comments.add(new TranslatorPoComment(token.value.substring(1)))
    if (token.value.startsWith("#~")) {
      val sb = new StringBuilder
      var done = false
      while (!done) {
        sb.append(token.value.substring(2).trim)
        sb.append("\n")
        done = !(currentChar == '#' && nextChar == '~')
        super.getToken
      }
      obsoleteComments.add(new ObsoletePoComment(sb.toString))
    } else
      super.getToken
  }

  override def getToken {
    super.getToken
    while (token is poLineComments)
      processComment
  }

  // Consider consecutive strings as a single string.
  def eatString = {
    val t = super.eatToken(CharacterString).value
    if (token is CharacterString) {
      val sb = new StringBuilder(t)
      do sb.append(super.eatToken(CharacterString)) while (token is CharacterString)
      sb.toString
    } else
      t
  }

  private def recoverEntry: PoEntry = {
    def eatWhen(sym: LexSymbol) = {
      if (token is sym) {
        getToken
        Some(eatString)
      } else {
        None
      }
    }
    val (fuzzy, accumulatedComments) = comments.obtainWithFuzzyAndClear
    val msgCtxt = eatWhen(Msgctxt)
    skip(Msgid)
    val msgidValue = eatString
    val msgidPlural = eatWhen(Msgid_plural)
    var lastIndex = -1
    val translations = ListBuffer[String]()
    expect(Msgstr)
    do {
      getToken
      val currentIndex =
        if (token is leftBracket) {
          getToken
          expect(Number)
          val numberString = token.value
          getToken
          expect(rightBracket)
          getToken
          numberString.toInt
        } else {
          0
        }
      if (currentIndex != lastIndex + 1)
        Errors.fatal("Expected index of translated message to be _, but got _." << (lastIndex + 1, currentIndex))
      lastIndex = currentIndex
      val t = eatString
      translations += t
    } while (token is Msgstr)
    if (Msgid == "") {
      if (emptyMsgidFound)
        Errors.fatal("More than one empty 'msgid' found in the Po file.")
      emptyMsgidFound = true
    }
    new PoEntry(accumulatedComments, List[PoReference](), msgCtxt, msgidValue, msgidPlural, translations.toList, fuzzy)
  }

  def parse = Errors.liveTrap("Line _" << token.lineNumber) {
    val entriesBuffer = ListBuffer[PoEntry]()
    var previousHeaderLine = 0
    var header: Option[PoEntry] = None
    getToken
    while (token isNot Eof) {
      val e = recoverEntry
      if (e.hasSomeTranslations)
        entriesBuffer += e
      if (e.msgid == "") {
        if (header != None)
          Errors.fatal("The Po file header entry (marked by a msgid \"\") appears twice, it first " +
            "appeared around line _." << previousHeaderLine)
        header = Some(e)
        previousHeaderLine = token.lineNumber
      }
    }
    (header, entriesBuffer.toList, obsoleteComments.obtainAndClear)
  }
}
