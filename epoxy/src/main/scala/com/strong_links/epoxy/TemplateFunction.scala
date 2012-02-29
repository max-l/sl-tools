package com.strong_links.epoxy

import com.strong_links.core._
import com.strong_links.core.lex._

class TemplateFunction(val templateCompiler: TemplateCompiler) {

  // We import the whole TemplateCompiler parent class. The alternative would have been to define TemplateFunciton as an
  // inner class of TemplateCompiler, but this would have led to an oversized source file.
  import templateCompiler._

  def isValidArgumentName(n: String) = n.startsWith("$") && n.length > 1 && (n(1).isLetter || n(1) == '_')

  def argumentNameError(n: String) = {
    Errors.fatal("Invalid argument name _ (it must start with a '$' and it must have a length of 2 or more)." << n)
  }

  def extractArgumentName(n: String) = {
    if (!isValidArgumentName(n))
      argumentNameError(n)
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
    var pc, di, ci, cu = false
    while (token in (PreserveComments, DisableInterpretation, CacheI18n, CacheUri))
      eatAnyToken.symbol match {
        case PreserveComments      => pc = true
        case DisableInterpretation => di = true
        case CacheI18n             => ci = true
        case CacheUri              => cu = true
        case _                     => Errors.badValue(token.symbol)
      }
    (pc, di, ci, cu)
  }

  def getArgumentMember(codeWriter: CodeWriter, flushIfFound: Boolean, mustExist: Boolean) = {
    expect(Identifier)
    if (isValidArgumentName(token.value)) {
      arguments.find(_.name == extractArgumentName(token.value)) match {
        case None =>
          if (mustExist)
            Errors.fatal("Unknown argument _." << token.value)
          None
        case Some(argument) =>
          if (flushIfFound)
            codeWriter.staticFlush(token)
          val startLineNumber = token.lineNumber
          skip(Identifier)
          val isObject = skipped(Dot)
          argument.usedAs(isObject, startLineNumber)
          val memberName = if (isObject) Some(eatToken(Identifier).value) else None
          Some(argument.searchMember(memberName))
      }
    } else {
      if (mustExist)
        argumentNameError(token.value)
      None
    }
  }

  def processDollarIdentifier(codeWriter: CodeWriter) {

    val startLineNumber = token.lineNumber

    getArgumentMember(codeWriter, true, false) match {

      case None =>
        getToken

      case Some(member) =>
        val usage = if (token is Colon) {
          skip(Colon)
          eatToken(Html, I18n_, I18nJs, Js, I18nJsHtml, JsHtml, Raw, Xml, Field, Label, Control, Help, Error, Uri, Template).symbol
        } else
          Html

        codeWriter.staticRestartAt(token)

        member.usedAs(usage, startLineNumber)

        member.optionAs(ifdefStack.exists(_._1 == member), startLineNumber)

        codeWriter.write(member.render(usage))
    }
  }

  def processHtmlComment(startToken: LexToken, codeWriter: CodeWriter) = {
    val startLineNumber = token.lineNumber
    codeWriter.staticFlush(startToken)
    def commentCommand(code: => Unit) {
      code
      skip(HtmlEndComment)
      codeWriter.staticRestartAt(token)
    }
    token.symbol match {
      case End =>
        commentCommand { skip(End) }
        (true, Some(false))
      case Def =>
        // Stay on token "Def" for the processing of the next function.
        (true, Some(true))
      case Ifdef =>
        commentCommand {
          skip(Ifdef)
          getArgumentMember(codeWriter, false, true) match {
            case None =>
              Errors.badLogic
            case Some(member) =>
              if (ifdefStack.exists(_._1 == member))
                Errors.fatal("An 'ifdef' directive is already active for _." << member.fullExternalMemberName)
              ifdefStack = (member, startLineNumber) :: ifdefStack
              member.optionAs(true, startLineNumber)
              codeWriter.pushCase(member)
          }
        }
        (false, None)
      case Endif =>
        commentCommand {
          skip(Endif)
          if (ifdefStack.isEmpty)
            Errors.fatal("Unmatched 'ifdef'.")
          ifdefStack = ifdefStack.tail
          codeWriter.popCase
        }
        (false, None)
      case _ =>
        commentCommand { findToken(HtmlEndComment) }
        (false, None)
    }
  }

  def parseFunctionBody = {
    // We get here with the first token of the function body, after the end of the html comment.
    var done = false
    val codeWriter = new CodeWriter(this, token)
    var hasNextFunction = false
    def processPair(leftSymbol: LexSymbol, rightSymbol: LexSymbol, f: String => Unit) {
      val leftToken = eatToken(leftSymbol)
      codeWriter.staticFlush(leftToken)
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
          processHtmlComment(eatToken(HtmlStartComment), codeWriter) match {
            case (d, None) if !d     =>
            case (d, Some(hnf)) if d => done = true; hasNextFunction = hnf
            case _                   => Errors.badLogic
          }
        case Eof =>
          codeWriter.staticFlush(token)
          done = true
        case LeftBrace if !disableInterpretation =>
          processPair(LeftBrace, RightBrace, codeWriter.writeI18n)
        case LeftBracket if !disableInterpretation =>
          processPair(LeftBracket, RightBracket, codeWriter.writeUri)
        case _ =>
          getToken
      }

    if (!ifdefStack.isEmpty)
      Errors.fatal("Unclosed 'ifdef' directive for _, which started at line _." <<
        (ifdefStack.head._1.fullExternalMemberName, ifdefStack.head._2))

    val unusedArguments = arguments.filter(_.members.isEmpty).map(_.externalName)
    if (!unusedArguments.isEmpty)
      Errors.fatal("These arguments are not used by the function: _" << unusedArguments)

    val undefinedArgumentMemberNames = arguments.flatMap(_.members.filter(!_.hasBaseType).map(_.fullExternalMemberName))
    if (!undefinedArgumentMemberNames.isEmpty)
      Errors.fatal("These argument members have an unknown type: _" << undefinedArgumentMemberNames)

    (codeWriter.generateCode, hasNextFunction)
  }

  def usesFieldTransformer = arguments.exists(_.usesFieldTransformer)

  def cleanComments(s: String): String = {
    val pos = s.indexOf(HtmlStartComment.special)
    if (pos == -1)
      s
    else {
      val z = s.indexOf(HtmlEndComment.special, pos + HtmlStartComment.special.length)
      if (z == -1)
        Errors.fatal("Unclosed HTML comment in _." << s)
      cleanComments(s.substring(0, pos) + s.substring(z + HtmlEndComment.special.length))
    }
  }

  def massage(s: String) = if (preserveComments) s else cleanComments(s)

  // Constructor start --

  var ifdefStack = List[(TemplateFunctionArgumentMember, Int)]()

  // Remember on which line this function appears.
  val lineNumber = token.lineNumber

  // We get here with a current token being "def", check that.
  skip(Def)

  // The next token is the function name.
  val name = getTemplateFunctionName

  // Get the function arguments, if any.
  val arguments = getTemplateFunctionArguments

  // Get options, if any.
  val (preserveComments, disableInterpretation, cacheI18n, cacheUri) = getOptions

  // Then expect eof of comment.
  skip(HtmlEndComment)

  // Then parse the function body itself.
  val (code, hasNextFunction) = parseFunctionBody
}

