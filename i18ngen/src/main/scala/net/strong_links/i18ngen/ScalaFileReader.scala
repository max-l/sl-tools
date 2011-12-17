package net.strong_links.i18ngen

import net.strong_links.core._
import net.strong_links.core.lex._

import java.io.File

class ScalaFileReader(file: File, entryBag: PoEntryBag) extends LexParser(IO.loadUtf8TextFile(file)) {

  val VerbatimString, ScalaLineComments, BlockComments = Value
  val I18nNeutral, I18nNonNeutral, I18nNeutralCtxt, I18nNonNeutralCtxt = Value

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
    def escape(c: Char) = {
      c match {
        case '\n' => "\\n"
        case '\'' => "\\'"
        case '"' => "\\\""
        case '\\' => "\\\\"
        case _ => c.toString
      }
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
    setToken(VerbatimString, t)
  }

  override def getWord(word: String) {
    word match {
      case "I18n" => setToken(I18nNeutral)
      case "I18nPlural" => setToken(I18nNonNeutral)
      case "I18nCtxt" => setToken(I18nNeutralCtxt)
      case "I18nPluralCtxt" => setToken(I18nNonNeutralCtxt)
      case _ => super.getWord(word)
    }
  }

  override def getMiscellaneous {
    if (currentChar == '"')
      getString
    else if (isBlockCommentStart)
      getBlockComments
    else if (isVerbatimQuotes)
      getVerbatimString
    else if (currentChar == '/' && nextChar == '/')
      getLineComments(ScalaLineComments)
    else
      super.getMiscellaneous
  }

  // Treat verbatim strings as normal strings.
  def eatString = {
    expect(VerbatimString, CharacterString)
    eatAnyToken
  }

  // Comments accumulated in the file. We ignore comments that are not within 5 lines of each other.
  val comments = new FlushableCommentBag(5)

  var entryStartLineNumber = 0

  def add(withContext: Boolean, withPlural: Boolean, lineNumber: Int) {
    val msgCtxt = if (withContext) { val ctx = eatString; skip(Comma); Some(ctx.value) } else None
    val msgidValue = eatString.value
    val msgPlural = if (withPlural) { skip(Comma); Some(eatString.value) } else None
    val reference = new PoReference(file.getAbsolutePath, entryStartLineNumber)
    val e = new PoI18nCall(comments.obtainAtLine(lineNumber), Some(reference), msgCtxt, msgidValue, msgPlural)
    entryBag.merge(e)
  }

  // We assume that it is a real I18n usage when the identified I18n symbol is followed by a right
  // parenthesis and a literal string. Else we simply ignore.
  def tryAdd(withContext: Boolean, withPlural: Boolean) {
    entryStartLineNumber = token.lineNumber
    getToken
    if (token is LeftParenthesis) {
      getToken
      if (token in (CharacterString, VerbatimString))
        add(withContext, withPlural, token.lineNumber)
    }
  }

  def scan {
    getToken
    while (token isNot Eof) {
      token.symbol match {
        case ScalaLineComments if token.value.startsWith("///") =>
          comments.addAtLine(new ScalaPoComment(token.value.substring(3)), token.lineNumber)
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
