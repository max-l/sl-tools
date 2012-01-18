package net.strong_links.i18ngen

import net.strong_links.core._
import net.strong_links.core.lex._

import java.io.File
import scala.collection.mutable.ListBuffer

class PoReaderParseResults(val poHeaderEntry: Po18nEntry, val poI18nEntries: List[Po18nEntry], val obsoleteComments: List[ObsoletePoComment])

class PoFileReader(file: File) extends PoReader(IO.loadUtf8TextFile(file))

class PoReader(data: String) extends LexParser(data) {

  // Line at which analysis found the start of a PoEntry
  var startLineNumber = token.lineNumber

  // Additional symbols required for the lexical analysis.
  val Msgid, Msgctxt, Msgid_plural, Msgstr = idSymbol
  val leftBracket = specialSymbol("[")
  val rightBracket = specialSymbol("]")
  val poLineComments = symbol

  // Objects that will be accumulated during the parsing.
  val comments = new PoComments
  val obsoleteComments = scala.collection.mutable.ListBuffer[ObsoletePoComment]()

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
      obsoleteComments.append(new ObsoletePoComment(sb.toString))
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
      do sb.append(super.eatToken(CharacterString).value) while (token is CharacterString)
      sb.toString
    } else
      t
  }

  private def getPo18nEntry: Po18nEntry = {
    def eatWhen(sym: LexSymbol) = {
      if (token is sym) {
        getToken
        Some(eatString)
      } else {
        None
      }
    }
    val (fuzzy, accumulatedComments) = comments.obtainWithFuzzyAndClear
    startLineNumber = token.lineNumber
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
    new Po18nEntry(msgCtxt, msgidValue, msgidPlural, accumulatedComments, translations.toList, Nil, fuzzy)
  }

  def whereItFailed = if (startLineNumber == token.lineNumber)
    "Line _" << startLineNumber
  else
    "Near lines _ to _" << (startLineNumber, token.lineNumber)

  def parse = Errors.liveTrap(whereItFailed) {
    val po18nEntries = scala.collection.mutable.ListBuffer[Po18nEntry]()
    getToken
    while (token isNot Eof)
      po18nEntries += getPo18nEntry

    val (emptyEntries, nonEmptyEntries) = po18nEntries.toList.partition(_.msgid == "")

    val headerPoEntry = emptyEntries match {
      case List(h) => h
      case Nil => Errors.fatal("No header found.")
      case list => Errors.fatal("_ headers found while only one was expected." << list.length)
    }

    new PoReaderParseResults(headerPoEntry, nonEmptyEntries, obsoleteComments.toList)
  }
}
