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
  val Package = idSymbol
  val Trait = idSymbol
  val Type = idSymbol
  val Import = idSymbol
  val _Class = idSymbol("class")
  val _Object = idSymbol("object")
  val LeftBrace = specialSymbol("{")
  val RightBrace = specialSymbol("}")
  val Plus = specialSymbol("+")
  val Semicolon = specialSymbol(";")

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
          pack: List[String], importedI18nCatalog: Option[List[String]], calls: Calls) {
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
    importedI18nCatalog match {
      case None => Errors.fatal("No imported i18nCatalog is in scope.")
      case Some(ic) =>
        calls += new SourceI18nCall(ic, msgCtxt, msgidValue, msgPlural, comments.obtainAtLine(lineNumber), file, entryStartLineNumber)
    }
  }

  // We assume that it is a real I18n usage when the identified I18n symbol is followed by a right
  // parenthesis and a literal string. Else we simply ignore.
  def tryAdd(withContext: Boolean, withPlural: Boolean, pack: List[String], importedI18nCatalog: Option[List[String]], calls: Calls) {
    val entryStartLineNumber = token.lineNumber
    getToken
    if (token is LeftParenthesis) {
      getToken
      if (token is CharacterString)
        add(withContext, withPlural, token.lineNumber, entryStartLineNumber, pack, importedI18nCatalog, calls)
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

  def processOtherToken(pack: List[String], importedI18nCatalog: Option[List[String]], calls: Calls) {
    token.symbol match {
      case I18nNeutral =>
        tryAdd(false, false, pack, importedI18nCatalog, calls)
      case I18nNeutralCtxt =>
        tryAdd(true, false, pack, importedI18nCatalog, calls)
      case I18nNonNeutral =>
        tryAdd(false, true, pack, importedI18nCatalog, calls)
      case I18nNonNeutralCtxt =>
        tryAdd(true, true, pack, importedI18nCatalog, calls)
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
    eatToken(Import)
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
      case None => None
    }
  }

  def parsePackage(fp: List[String], importedI18nCatalog: Option[List[String]], calls: Calls) = {
    var currentImportedI18nCatalog = importedI18nCatalog
    while (token isNot Eof)
      if (token is LeftBrace)
        parseBlock(fp, Nil, currentImportedI18nCatalog, calls)
      else if (token is Package) {
        val pi = getPackageInfo
        if (pi.isBlock)
          parseBlock(fp, pi.packageSegments, currentImportedI18nCatalog, calls)
      } else if (token is Import)
        currentImportedI18nCatalog = processImport(currentImportedI18nCatalog)
      else
        processOtherToken(fp, currentImportedI18nCatalog, calls)
  }

  def parseBlock(fp: List[String], bp: List[String], importedI18nCatalog: Option[List[String]], calls: Calls) {
    var currentImportedI18nCatalog = importedI18nCatalog
    skip(LeftBrace)
    val pack = fp ::: bp
    while ((token isNot RightBrace) && (token isNot Eof)) {
      if (token is LeftBrace) {
        parseBlock(fp, bp, currentImportedI18nCatalog, calls)
      } else if (token is Package) {
        val pi = getPackageInfo
        if (pi.isBlock)
          parseBlock(fp, bp ::: pi.packageSegments, currentImportedI18nCatalog, calls)
      } else if (token is Import)
        currentImportedI18nCatalog = processImport(currentImportedI18nCatalog)
      else
        processOtherToken(pack, currentImportedI18nCatalog, calls)
    }
    eatToken(RightBrace)
  }

  case class PackageInfo(packageSegments: List[String], objectPackage: Boolean, isBlock: Boolean)

  def getPackageInfo = {
    skip(Package)
    val objectPackage = token is _Object
    if (objectPackage)
      getToken
    val packageSegments = eatPackageSegments
    while (token notIn (Semicolon, Trait, _Class, Type, Eof, Package, LeftBrace, RightBrace, _Object, Import))
      getToken
    val isBlock = token is LeftBrace
    if (token is Semicolon)
      getToken
    PackageInfo(packageSegments, objectPackage, isBlock)
  }

  def getInitialPackages = {
    var fp: List[String] = Nil
    var bp: List[String] = Nil
    var blockFound = false
    while ((token is Package) && !blockFound) {
      val pi = getPackageInfo
      blockFound = pi.isBlock
      if (blockFound)
        bp = bp ::: pi.packageSegments
      else
        fp = fp ::: pi.packageSegments
    }
    (fp, bp)
  }

  def parse = Errors.liveTrap("_, around line _" << (file, token.lineNumber)) {

    val calls = new Calls()
    getToken

    // Get the file package and the first block package. Both can be Nil here.
    val (fp, bp) = getInitialPackages

    if (bp == Nil)
      parsePackage(fp, None, calls)
    else {
      parseBlock(fp, bp, None, calls)
      parsePackage(fp, None, calls)
    }
    expect(Eof)

    calls.toList
  }
}
