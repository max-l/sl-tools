package net.strong_links.i18ngen

import net.strong_links.core._
import net.strong_links.core.lex._
import java.io.File
import net.strong_links.core.LoggingPrefixed

class ScalaFileReader(file: File) extends LexParser(IO.loadUtf8TextFile(file)) with LoggingPrefixed {

  val loggingPrefixSeq = Seq(file: FileLoggingParameter)

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

  def add(withContext: Boolean, withPlural: Boolean, lineNumber: Int, entryStartLineNumber: Int, pack: Option[String], calls: Calls) {
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
    calls += new ScalaI18nCall(pack, msgCtxt, msgidValue, msgPlural, comments.obtainAtLine(lineNumber), file, entryStartLineNumber)
  }

  // We assume that it is a real I18n usage when the identified I18n symbol is followed by a right
  // parenthesis and a literal string. Else we simply ignore.
  def tryAdd(withContext: Boolean, withPlural: Boolean, pack: Option[String], calls: Calls) {
    val entryStartLineNumber = token.lineNumber
    getToken
    if (token is LeftParenthesis) {
      getToken
      if (token is CharacterString)
        add(withContext, withPlural, token.lineNumber, entryStartLineNumber, pack, calls)
    }
  }

  def eatPackageName = {
    expect(Identifier)
    var packageName = new StringBuilder(token.value)
    getToken
    while (token is Dot) {
      getToken
      expect(Identifier)
      packageName += '.'
      packageName.append(token.value)
      getToken
    }
    packageName.toString
  }

  def eatPackage = {
    skip(Package)
    // For our purpose, a "package" or a "package object" is the same, so just ignore any "object" here.
    if (token is _Object)
      getToken
    val packageName = eatPackageName
    I18nConfig.checkPackageSegments(Util.split(packageName, '.'))
    packageName
  }

  def cat(a: Option[String], b: Option[String]) = (a, b) match {
    case (None, None) => None
    case (Some(a), None) => Some(a)
    case (None, Some(b)) => Some(b)
    case (Some(a), Some(b)) => Some(a + "." + b)
  }

  def eatInitialPackages = {
    def makePack(segments: List[String]) = segments match {
      case Nil => None
      case s => Some(s.mkString("."))
    }
    val packageNamesBuffer = scala.collection.mutable.ListBuffer(eatPackage)
    while (token is Package)
      packageNamesBuffer += eatPackage
    val packageNames = packageNamesBuffer.toList
    val (fps, bps) = if (token is LeftBrace)
      (packageNames.dropRight(1), List(packageNames.last))
    else
      (packageNames, Nil)
    (makePack(fps), makePack(bps))
  }

  def processToken(pack: Option[String], calls: Calls) {
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

  def parsePackage(fp: Option[String], calls: Calls) = {
    logDebug("File package _" << fp)
    while (token isNot Eof)
      if (token is Package)
        parseBlockPackage(fp, Some(eatPackage), calls)
      else
        processToken(fp, calls)
  }

  def parseBlockPackage(fp: Option[String], bp: Option[String], calls: Calls) {
    logDebug("Block package _" << bp)
    skip(LeftBrace)
    var level = 1
    val pack = cat(fp, bp)
    while (level != 0) {
      if (token is LeftBrace) {
        level += 1
        getToken
      } else if (token is RightBrace) {
        level -= 1
        getToken
      } else if (token is Package)
        parseBlockPackage(fp, cat(bp, Some(eatPackage)), calls)
      else
        processToken(pack, calls)
    }
  }

  def parse = Errors.liveTrap("_, around line _" << (file, token.lineNumber)) {

    logDebug("Parsing started.")

    val calls = new Calls()
    getToken

    // Get the file package and the first block package. Both can be None here.
    val (fp, bp) = eatInitialPackages

    logDebug("Starting with _ and _" << (fp, bp))

    if (bp == None)
      parsePackage(fp, calls)
    else {
      parseBlockPackage(fp, bp, calls)
      parsePackage(fp, calls)
    }
    expect(Eof)

    val results = calls.toList

    logDebug("Parsing ended.")

    results
  }
}
