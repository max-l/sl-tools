package com.strong_links.i18ngen

import com.strong_links.core._
import com.strong_links.core.lex._
import java.io.File

class AlternatePoFileReader(file: File) extends LexParser(IO.loadUtf8TextFile(file)) with Logging {

  type Calls = scala.collection.mutable.ListBuffer[SourceI18nCall]

  val ScalaLineComments, BlockComments = symbol
  val I18nNeutral = idSymbol("I18n")
  val I18nNonNeutral = idSymbol("I18nPlural")
  val I18nNeutralCtxt = idSymbol("I18nCtxt")
  val I18nNonNeutralCtxt = idSymbol("I18nPluralCtxt")
  val Package = idSymbol
  val Extends = idSymbol
  val _Object = idSymbol("object")
  val LeftBrace = specialSymbol("{")
  val RightBrace = specialSymbol("}")
  val Plus = specialSymbol("+")
  val LeftBracket = specialSymbol("[")
  val RightBracket = specialSymbol("]")

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
    val s = eatToken(CharacterString).value
    def eatConcatenatedString = {
      val b = new StringBuilder(s)
      while (token is Plus) {
        getToken
        b.append(eatToken(CharacterString).value)
      }
      b.toString
    }
    if (token is Plus)
      eatConcatenatedString
    else
      s
  }

  // Comments accumulated in the file. We ignore comments that are not within 5 lines of each other.
  val comments = new ScalaComments(5)

  override def getToken = {
    super.getToken
    while ((token is ScalaLineComments) || (token is BlockComments)) {
      if (token.value.startsWith("///"))
        comments.addAtLine(new ScalaComment(token.value.substring(3)), token.lineNumber)
      super.getToken
    }
  }

  val blockStartSymbols = Set(Package, LeftParenthesis, LeftBrace, LeftBracket, Eof)

  def parseBlock(packageSegments: List[String], level: Int, calls: Calls, startSymbol: LexSymbol, endSymbol: LexSymbol) {

    def add(withContext: Boolean, withPlural: Boolean, lineNumber: Int, entryStartLineNumber: Int, pack: List[String], calls: Calls) {
      val msgCtxt = if (withContext) { val ctx = eatString; skip(Comma); Some(ctx) } else None
      val msgidValue = eatString
      val msgPlural = if (withPlural) {
        skip(Comma)
        if (token is CharacterString)
          Some(eatString)
        else
          Some(msgidValue)
      } else
        None
      if (pack == Nil)
        Errors.fatal("No package is in scope.")
      calls += new SourceI18nCall(pack, msgCtxt, msgidValue, msgPlural, comments.obtainAtLine(lineNumber), file, entryStartLineNumber)
    }

    // We assume that it is a real I18n usage when the identified I18n symbol is followed by a right
    // parenthesis and a literal string. Else we simply ignore.
    def tryAdd(withContext: Boolean, withPlural: Boolean) {
      Errors.fatal("try")
      val entryStartLineNumber = token.lineNumber
      getToken
      if (token is LeftParenthesis) {
        getToken
        if (token is CharacterString)
          add(withContext, withPlural, token.lineNumber, entryStartLineNumber, packageSegments, calls)
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

    def processToken {
      token.symbol match {
        case I18nNeutral =>
          tryAdd(false, false)
        case I18nNeutralCtxt =>
          tryAdd(true, false)
        case I18nNonNeutral =>
          tryAdd(false, true)
        case I18nNonNeutralCtxt =>
          tryAdd(true, true)
        case _ =>
          debug("Skipping _" << token)
          getToken
      }
    }

    def eatPackage = {
      skip(Package)
      if (token is _Object)
        getToken
      eatPackageSegments
    }

    def debug(params: LoggingParameter*) {
      val margin = "  " * level
      val info: String = "Level _ / " << (level)
      val line = margin + info + LoggingParameter.format(params)
      println(line)
    }

    debug("Start block for symbols _ and _ / token is _" << (startSymbol, endSymbol, token))

    var done = false

    do {
      token.symbol match {
        case Package =>
          debug("case process package")
          val evenMorePackageSegments = eatPackage
          val newPackageSegments = packageSegments ::: evenMorePackageSegments
          println("File _, Package _, level _" << (file, newPackageSegments, level))
          parseBlock(newPackageSegments, level + 1, calls, Package, Eof)
        case LeftParenthesis =>
          getToken
          parseBlock(packageSegments, level + 1, calls, LeftParenthesis, RightParenthesis)
          getToken
        case LeftBrace =>
          getToken
          parseBlock(packageSegments, level + 1, calls, LeftBrace, RightBrace)
          getToken
        case LeftBracket =>
          getToken
          parseBlock(packageSegments, level + 1, calls, LeftBracket, RightBracket)
          getToken
        case sym if sym == endSymbol =>
          debug("endSymbol _" << sym)
          getToken
        case Eof =>
          expect(endSymbol)
        case _ =>
          processToken
      }
    } while (!done)

    debug("End block for symbols _ and _ / token is _" << (startSymbol, endSymbol, token))
  }

  def parse = Errors.liveTrap("_, around line _" << (file, token.lineNumber)) {

    val calls = new Calls()
    getToken
    parseBlock(Nil, 0, calls, Other, Eof)
    expect(Eof)
    calls.toList
    Errors.fatal("endsss")
  }
}

