package com.strong_links.epoxy

import com.strong_links.core._
import com.strong_links.core.lex._

class TemplateFunction(val templateCompiler: TemplateCompiler) {

  import templateCompiler._

  def getFunctionName = {
    val name = eatToken(Identifier).value
    if (!name(0).isLower)
      Errors.fatal("The function name _ does not start with a lowercase letter." << name)
    if (templateFunctions.contains(name))
      Errors.fatal("The function _ has already been declared near line _." << (name, templateFunctions(name).lineNumber))
    name
  }

  def getTemplateFunctionArguments = {
    var b = scala.collection.mutable.ListBuffer[TemplateFunctionArgument]()
    def grabArgument {
      val argumentName = eatToken(Identifier).value
      if (!argumentName.startsWith("$"))
        Errors.fatal("The argument name _ does not start with a $ sign." << argumentName)
      if (argumentName.length == 1)
        Errors.fatal("The argument name _ is invalid; letters are required." << argumentName)
      b += new TemplateFunctionArgument(this, argumentName.substring(1))
    }
    if (token is LeftParenthesis) {
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

  def processReference(codeWriter: CodeWriter, argumentToken: LexToken,
                       templateFunctionArgument: TemplateFunctionArgument, lineNumber: Int) {

    // Check if first use, and if so, whether the usage as object/non object is consistent.
    val isObject = skipped(Dot)

    templateFunctionArgument.usedAs(isObject, lineNumber)

    // Get the member name. 
    val memberName = if (isObject) Some(eatToken(Identifier).value) else None

    val usage = if (token is Colon) {
      skip(Colon)
      eatToken(String, I18n_, I18nJs, Js, Raw, Xml, Field, Label, Control, Help, Error, Uri).symbol
    } else
      String

    codeWriter.flush(argumentToken, token)

    val member = templateFunctionArgument.addMember(memberName, usage, lineNumber)

    codeWriter.write(member.render(usage))
  }

  def processDollar(codeWriter: CodeWriter) {
    // We enter here with an $identifier. Process it only if we know it.
    val argumentToken = eatToken(Identifier)
    val argumentName = argumentToken.value.substring(1)
    arguments.find(_.name == argumentName) match {
      case None =>
      case Some(argument) =>
        processReference(codeWriter, argumentToken, argument, lineNumber)
    }
  }

  def parseFunctionBody = {
    // We get here with the first token of the function body, after the end of the html comment.
    var done = false
    val codeWriter = new CodeWriter(this, token)
    var hasNextFunction = false
    def processPair(leftSymbol: LexSymbol, rightSymbol: LexSymbol, f: String => Unit) {
      val leftToken = eatToken(leftSymbol)
      findToken(rightSymbol)
      val rightToken = eatToken(rightSymbol)
      val x = getDataBetween(leftToken, false, rightToken, false, false)
      codeWriter.flush(leftToken, token)
      f(x)
    }
    while (!done)
      token.symbol match {
        case Identifier if (isArgumentName(token.value)) =>
          processDollar(codeWriter)
        case HtmlStartComment =>
          val htmlStartToken = eatAnyToken
          if (token is End) {
            skip(End)
            eatToken(HtmlEndComment)
            codeWriter.flush(htmlStartToken)
            done = true
          } else if (token is Def) {
            // Stay on token "Def" for the processing of the next function.
            codeWriter.flush(htmlStartToken)
            done = true
            hasNextFunction = true
          } else {
            findToken(HtmlEndComment)
            skip(HtmlEndComment)
          }
        case Eof =>
          codeWriter.flush(token)
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

  var ifdefLevel = 0

  // Remember on which line this funciton appears.
  val lineNumber = token.lineNumber

  // We get here with a current token being "def", check that.
  eatToken(Def)

  // The next token is the function name.
  val name = getFunctionName

  // Get the function arguments, if any.
  val arguments = getTemplateFunctionArguments

  // Get options, if any.
  val (preserveSpace, disableInterpretation, cacheI18n, cacheUri) = getOptions

  // Then expect eof of comment.
  skip(HtmlEndComment)

  val (code, hasNextFunction) = parseFunctionBody
}
