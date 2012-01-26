package net.strong_links.i18ngen

import net.strong_links.core._
import net.strong_links.core.lex._
import java.io.File

class ScalaFileReader(file: File) extends LexParser(IO.loadUtf8TextFile(file)) with Logging {

  type Calls = scala.collection.mutable.ListBuffer[ScalaI18nCall]

  val ScalaLineComments, BlockComments = symbol
  val I18nNeutral = idSymbol("I18n")
  val I18nNonNeutral = idSymbol("I18nPlural")
  val I18nNeutralCtxt = idSymbol("I18nCtxt")
  val I18nNonNeutralCtxt = idSymbol("I18nPluralCtxt")
  val Package = idSymbol
  val _Object = idSymbol("object")
  val LeftBrace = specialSymbol("{")
  val RightBrace = specialSymbol("}")

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

  def add(withContext: Boolean, withPlural: Boolean, lineNumber: Int, entryStartLineNumber: Int, pack: List[String], calls: Calls) {
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
    if (pack == Nil)
      Errors.fatal("No package is in scope.")
    calls += new ScalaI18nCall(pack, msgCtxt, msgidValue, msgPlural, comments.obtainAtLine(lineNumber), file, entryStartLineNumber)
  }

  // We assume that it is a real I18n usage when the identified I18n symbol is followed by a right
  // parenthesis and a literal string. Else we simply ignore.
  def tryAdd(withContext: Boolean, withPlural: Boolean, pack: List[String], calls: Calls) {
    val entryStartLineNumber = token.lineNumber
    getToken
    if (token is LeftParenthesis) {
      getToken
      if (token is CharacterString)
        add(withContext, withPlural, token.lineNumber, entryStartLineNumber, pack, calls)
    }
  }

  def eatPackageSegments = {
    expect(Identifier)
    var packageSegments = scala.collection.mutable.ListBuffer(token.value)
    getToken
    while (token is Dot) {
      getToken
      expect(Identifier)
      packageSegments += token.value
      getToken
    }
    packageSegments.toList
  }

  def eatPackage = {
    skip(Package)
    if (token is _Object)
      getToken
    eatPackageSegments
  }

  def cat(a: Option[String], b: Option[String]) = (a, b) match {
    case (None, None) => None
    case (Some(a), None) => Some(a)
    case (None, Some(b)) => Some(b)
    case (Some(a), Some(b)) => Some(a + "." + b)
  }

  def eatInitialPackages = {
    var list = eatPackage
    while (token is Package)
      list = list ::: eatPackage
    if (token is LeftBrace)
      (list.dropRight(1), List(list.last))
    else
      (list, Nil)
  }

  def processToken(pack: List[String], calls: Calls) {
    token.symbol match {
      case ScalaLineComments if token.value.startsWith("///") =>
        comments.addAtLine(new ScalaComment(token.value.substring(3)), token.lineNumber)
        getToken
      case I18nNeutral =>
        tryAdd(false, false, pack, calls)
      case I18nNeutralCtxt =>
        tryAdd(true, false, pack, calls)
      case I18nNonNeutral =>
        tryAdd(false, true, pack, calls)
      case I18nNonNeutralCtxt =>
        tryAdd(true, true, pack, calls)
      case _ =>
        getToken
    }
  }

  def parsePackage(fp: List[String], calls: Calls) = {
    while (token isNot Eof)
      if (token is Package)
        parseBlockPackage(fp, eatPackage, calls)
      else
        processToken(fp, calls)
  }

  def parseBlockPackage(fp: List[String], bp: List[String], calls: Calls) {
    skip(LeftBrace)
    var level = 1
    val pack = fp ::: bp
    while (level != 0) {
      if (token is LeftBrace) {
        level += 1
        getToken
      } else if (token is RightBrace) {
        level -= 1
        getToken
      } else if (token is Package)
        parseBlockPackage(fp, bp ::: eatPackage, calls)
      else
        processToken(pack, calls)
    }
  }

  def parse = Errors.liveTrap("_, around line _" << (file, token.lineNumber)) {

    val calls = new Calls()
    getToken

    // Get the file package and the first block package. Both can be Nil here.
    val (fp, bp) = eatInitialPackages

    if (bp == Nil)
      parsePackage(fp, calls)
    else {
      parseBlockPackage(fp, bp, calls)
      parsePackage(fp, calls)
    }
    expect(Eof)

    calls.toList
  }
}
