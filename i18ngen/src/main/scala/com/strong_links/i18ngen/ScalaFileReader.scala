package com.strong_links.i18ngen

// import com.strong_links.scalaforms_demo.i18nCatalog._

import com.strong_links.core._
import com.strong_links.core.lex._
import java.io.File

class ScalaFileReader(file: File) extends LexParser(IO.loadUtf8TextFile(file)) with Logging {

  type Calls = scala.collection.mutable.ListBuffer[SourceI18nCall]

  val ScalaLineComments = symbol
  val I18nNeutral = idSymbol("i18n")
  val I18nNonNeutral = idSymbol("i18nPlural")
  val I18nNeutralCtxt = idSymbol("i18nCtxt")
  val I18nNonNeutralCtxt = idSymbol("i18nPluralCtxt")
  val Import = idSymbol
  val LeftBrace = specialSymbol("{")
  val RightBrace = specialSymbol("}")
  val Plus = specialSymbol("+")

  def isVerbatimQuotes = currentChar == '"' && nextChar == '"' && nextNextChar == '"'

  private def getVerbatimString {
    def escape(c: Char) = c match {
      case '\n' => "\\n"
      case '\'' => "\\'"
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case _    => c.toString
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

  def add(withContext: Boolean, withPlural: Boolean, lineNumber: Int, entryStartLineNumber: Int,
          importedI18nCatalog: Option[List[String]], calls: Calls) {
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
    importedI18nCatalog match {
      case None => Errors.fatal("No imported i18nCatalog is in scope.")
      case Some(ic) =>
        calls += new SourceI18nCall(ic, msgCtxt, msgidValue, msgPlural, comments.obtainAtLine(lineNumber), file, entryStartLineNumber)
    }
  }

  // We assume that it is a real I18n usage when the identified I18n symbol is followed by a right
  // parenthesis and a literal string. Else we simply ignore.
  def tryAdd(withContext: Boolean, withPlural: Boolean, importedI18nCatalog: Option[List[String]], calls: Calls) {
    val entryStartLineNumber = token.lineNumber
    getToken
    if (token is LeftParenthesis) {
      getToken
      if (token is CharacterString)
        add(withContext, withPlural, token.lineNumber, entryStartLineNumber, importedI18nCatalog, calls)
    }
  }

  def processOtherToken(importedI18nCatalog: Option[List[String]], calls: Calls) {
    token.symbol match {
      case I18nNeutral =>
        tryAdd(false, false, importedI18nCatalog, calls)
      case I18nNeutralCtxt =>
        tryAdd(true, false, importedI18nCatalog, calls)
      case I18nNonNeutral =>
        tryAdd(false, true, importedI18nCatalog, calls)
      case I18nNonNeutralCtxt =>
        tryAdd(true, true, importedI18nCatalog, calls)
      case _ =>
        getToken
    }
  }

  def eatImportSegments: Option[List[String]] = {
    if (token isNot Identifier)
      return None
    var packageSegments = scala.collection.mutable.ListBuffer(token.value)
    getToken
    while (token is Dot) {
      getToken
      if (token isNot Identifier)
        return None
      packageSegments += token.value
      getToken
    }
    Some(packageSegments.toList)
  }

  def processImport(importedI18nCatalog: Option[List[String]]) = {
    eatImportSegments match {
      case Some(segments) =>
        if (segments.takeRight(2) == List("i18nCatalog", "_")) {
          val importedPackageNameSegments = segments.dropRight(2)
          I18nConfig.checkPackageSegments(importedPackageNameSegments)
          importedI18nCatalog match {
            case None =>
            case Some(ic) => Errors.fatal("Multiple i18nCatalog objects in scope: _ and _." <<
              (importedPackageNameSegments.mkString("."), ic.mkString(".")))
          }
          Some(importedPackageNameSegments)
        } else
          importedI18nCatalog
      case None =>
        None
    }
  }

  def parseBlock(endSymbol: LexSymbol, importedI18nCatalog: Option[List[String]], calls: Calls) {
    var currentImportedI18nCatalog = importedI18nCatalog
    while ((token isNot endSymbol) && (token isNot Eof))
      token.symbol match {
        case LeftBrace =>
          getToken
          parseBlock(RightBrace, currentImportedI18nCatalog, calls)
          eatToken(RightBrace)
        case Import =>
          getToken
          currentImportedI18nCatalog = processImport(currentImportedI18nCatalog)
        case I18nNeutral =>
          tryAdd(false, false, importedI18nCatalog, calls)
        case I18nNeutralCtxt =>
          tryAdd(true, false, importedI18nCatalog, calls)
        case I18nNonNeutral =>
          tryAdd(false, true, importedI18nCatalog, calls)
        case I18nNonNeutralCtxt =>
          tryAdd(true, true, importedI18nCatalog, calls)
        case _ =>
          getToken
      }
  }

  def parse = Errors.liveTrap("_, around line _" << (file, token.lineNumber)) {

    val calls = new Calls()

    getToken

    parseBlock(Eof, None, calls)

    expect(Eof)

    calls.toList
  }
}
