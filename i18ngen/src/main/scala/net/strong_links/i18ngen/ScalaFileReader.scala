package net.strong_links.i18ngen

import net.strong_links.core._
import net.strong_links.core.lex._

import java.io.File

class ScalaFileReader(file: File, scalaI18nCalls: scala.collection.mutable.ListBuffer[ScalaI18nCall]) extends LexParser(IO.loadUtf8TextFile(file)) {

  val ScalaLineComments, BlockComments = symbol
  val I18nNeutral = idSymbol("I18n")
  val I18nNonNeutral = idSymbol("I18nPlural")
  val I18nNeutralCtxt = idSymbol("I18nCtxt")
  val I18nNonNeutralCtxt = idSymbol("I18nPluralCtxt")

  def isVerbatimQuotes = currentChar == '"' && nextChar == '"' && nextNextChar == '"'

  private def isBlockCommentStart = currentChar == '/' && nextChar == '*'

  private def isBlockCommentEnd = currentChar == '*' && nextChar == '/'

  private def getBlockComments {
    val start = pos
    move(2)
    var level = 1
    do {
      eatUntil(isBlockCommentStart || isBlockCommentEnd)
      if (isBlockCommentStart)
        level += 1
      else
        level -= 1
      move(2)
    } while (level != 0)
    setToken(BlockComments, data.substring(start, pos))
  }

  private def getVerbatimString {
    def escape(c: Char) = c match {
      case '\n' => "\\n"
      case '\'' => "\\'"
      case '"' => "\\\""
      case '\\' => "\\\\"
      case _ => c.toString
    }
    move(3)
    val s = eatUntil(isVerbatimQuotes)
    move(3)
    // Exception in verbatim strings: characters such as \n and \t and
    // taken "as is" (two distinct characters), but the escape sequence
    // \u0000 is actually understood. So we need to do a bit of gymnastic to
    // deal with this.
    val t = s.replace("\\\\", "\uFFFF").
      replace("\\u", "\uFFFE").
      replace("\uFFFF", "\\\\").
      map(escape).
      mkString.
      replace("\uFFFE", "\\u")
    setToken(CharacterString, t)
  }

  override def getMiscellaneous {
    if (isVerbatimQuotes)
      getVerbatimString
    else if (currentChar == '"')
      getString
    else if (isBlockCommentStart)
      getBlockComments
    else if (currentChar == '/' && nextChar == '/')
      getLineComments(ScalaLineComments)
    else
      super.getMiscellaneous
  }

  def eatString = {
    expect(CharacterString)
    eatAnyToken
  }

  // Comments accumulated in the file. We ignore comments that are not within 5 lines of each other.
  val comments = new ScalaComments(5)

  def add(withContext: Boolean, withPlural: Boolean, lineNumber: Int, entryStartLineNumber: Int) {
    val msgCtxt = if (withContext) { val ctx = eatString; skip(Comma); Some(ctx.value) } else None
    val msgidValue = eatString.value
    val msgPlural = if (withPlural) {
      skip(Comma)
      if (token is CharacterString)
        Some(eatString.value)
      else
        Some(msgidValue)
    } else
      None
    scalaI18nCalls += new ScalaI18nCall(msgCtxt, msgidValue, msgPlural, comments.obtainAtLine(lineNumber), file, entryStartLineNumber)
  }

  // We assume that it is a real I18n usage when the identified I18n symbol is followed by a right
  // parenthesis and a literal string. Else we simply ignore.
  def tryAdd(withContext: Boolean, withPlural: Boolean) {
    val entryStartLineNumber = token.lineNumber
    getToken
    if (token is LeftParenthesis) {
      getToken
      if (token is CharacterString)
        add(withContext, withPlural, token.lineNumber, entryStartLineNumber)
    }
  }

  def parse = Errors.liveTrap("_, around line _" << (file, token.lineNumber)) {
    getToken
    while (token isNot Eof) {
      token.symbol match {
        case ScalaLineComments if token.value.startsWith("///") =>
          comments.addAtLine(new ScalaComment(token.value.substring(3)), token.lineNumber)
          getToken
        case I18nNeutral =>
          tryAdd(false, false)
        case I18nNeutralCtxt =>
          tryAdd(true, false)
        case I18nNonNeutral =>
          tryAdd(false, true)
        case I18nNonNeutralCtxt =>
          tryAdd(true, true)
        case _ =>
          getToken
      }
    }
  }
}
