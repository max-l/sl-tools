package com.strong_links.epoxy

import com.strong_links.core._
import com.strong_links.core.lex._

class TemplateFunction(val templateCompiler: TemplateCompiler) {

  // We import the whole TemplateCompiler parent class. The alternative would have been to define TemplateFunciton as an
  // inner class of TemplateCompiler, but this would have led to an oversized source file.
  import templateCompiler._

  def isValidArgumentName(n: String) = n.startsWith("$") && n.length > 1 && (n(1).isLetter || n(1) == '_')

  def extractArgumentName(n: String) = {
    if (!isValidArgumentName(n))
      Errors.fatal("Invalid argument name _ (must start with a $ and must have a length of 2 or more." << n)
    n.substring(1)
  }

  def getTemplateFunctionName = {
    val name = eatToken(Identifier).value
    if (!name(0).isLower)
      Errors.fatal("The function name _ does not start with a lowercase letter." << name)
    if (templateFunctions.contains(name))
      Errors.fatal("The function _ has already been declared near line _." << (name, templateFunctions(name).lineNumber))
    name
  }

  def getTemplateFunctionArguments = {
    var b = scala.collection.mutable.ListBuffer[TemplateFunctionArgument]()
    if (token is LeftParenthesis) {
      def grabArgument { b += new TemplateFunctionArgument(this, extractArgumentName(eatToken(Identifier).value)) }
      skip(LeftParenthesis)
      grabArgument
      while (token is Comma) {
        skip(Comma)
        grabArgument
      }
      skip(RightParenthesis)
    }
    b.toList
  }

  def getOptions = {
    var ps, di, ci, cu = false
    while (token in (PreserveSpace, DisableInterpretation, CacheI18n, CacheUri))
      eatAnyToken.symbol match {
        case PreserveSpace         => ps = true
        case DisableInterpretation => di = true
        case CacheI18n             => ci = true
        case CacheUri              => cu = true
        case _                     => Errors.badValue(token.symbol)
      }
    (ps, di, ci, cu)
  }

  def getArgumentMember(codeWriter: CodeWriter, flushIfFound: Boolean) = {
    expect(Identifier)
    if (isValidArgumentName(token.value)) {
      arguments.find(_.name == extractArgumentName(token.value)) match {
        case None =>
          None
        case Some(argument) =>
          if (flushIfFound)
            codeWriter.staticFlush(token, false)
          val startLineNumber = token.lineNumber
          skip(Identifier)
          val isObject = skipped(Dot)
          argument.usedAs(isObject, startLineNumber)
          val memberName = if (isObject) Some(eatToken(Identifier).value) else None
          Some(argument.searchMember(memberName))
      }
    } else
      None
  }

  def processDollarIdentifier(codeWriter: CodeWriter) {

    val startLineNumber = token.lineNumber

    getArgumentMember(codeWriter, true) match {

      case None =>
        getToken

      case Some(member) =>
        val usage = if (token is Colon) {
          skip(Colon)
          eatToken(String, I18n_, I18nJs, Js, Raw, Xml, Field, Label, Control, Help, Error, Uri).symbol
        } else
          String

        codeWriter.staticRestartAt(token)

        member.usedAs(usage, startLineNumber)

        codeWriter.write(member.render(usage))
    }
  }

  def parseFunctionBody = {
    // We get here with the first token of the function body, after the end of the html comment.
    var done = false
    val codeWriter = new CodeWriter(this, token)
    var hasNextFunction = false
    def processPair(leftSymbol: LexSymbol, rightSymbol: LexSymbol, f: String => Unit) {
      val leftToken = eatToken(leftSymbol)
      codeWriter.staticFlush(leftToken, false)
      findToken(rightSymbol)
      val rightToken = eatToken(rightSymbol)
      codeWriter.staticRestartAt(token)
      val x = getDataBetween(leftToken, false, rightToken, false, false)
      f(x)
    }
    while (!done)
      token.symbol match {
        case Identifier if (token.value.startsWith("$")) =>
          processDollarIdentifier(codeWriter)
        case HtmlStartComment =>
          val htmlStartToken = eatAnyToken
          if (token is End) {
            codeWriter.staticFlush(htmlStartToken, false)
            skip(End)
            eatToken(HtmlEndComment)
            codeWriter.staticRestartAt(token)
            done = true
          } else if (token is Def) {
            // Stay on token "Def" for the processing of the next function.
            codeWriter.staticFlush(htmlStartToken, true)
            done = true
            hasNextFunction = true
            //          } else if (token is Ifdef) {
            //          } else if (token is Endif) {
          } else {
            findToken(HtmlEndComment)
            skip(HtmlEndComment)
          }
        case Eof =>
          codeWriter.staticFlush(token, true)
          done = true
        case LeftBrace if !disableInterpretation =>
          processPair(LeftBrace, RightBrace, codeWriter.writeI18n)
        case LeftBracket if !disableInterpretation =>
          processPair(LeftBracket, RightBracket, codeWriter.writeUri)
        case _ =>
          getToken
      }

    if (ifdefLevel != 0)
      Errors.fatal("Unclosed 'ifdef'.")

    checkForUnusedArguments

    (codeWriter.generateCode, hasNextFunction)
  }

  def checkForUnusedArguments {
    val unusedArguments = arguments.filter(_.members.isEmpty).map(_.name)
    if (!unusedArguments.isEmpty)
      Errors.fatal("Unused arguments: _" << unusedArguments)
  }

  def usesFieldTransformer = arguments.exists(_.usesFieldTransformer)

  // Constructor start --

  var ifdefLevel = 0

  // Remember on which line this function appears.
  val lineNumber = token.lineNumber

  // We get here with a current token being "def", check that.
  skip(Def)

  // The next token is the function name.
  val name = getTemplateFunctionName

  // Get the function arguments, if any.
  val arguments = getTemplateFunctionArguments

  // Get options, if any.
  val (preserveSpace, disableInterpretation, cacheI18n, cacheUri) = getOptions

  // Then expect eof of comment.
  skip(HtmlEndComment)

  // Then parse the function body itself.
  val (code, hasNextFunction) = parseFunctionBody
}

