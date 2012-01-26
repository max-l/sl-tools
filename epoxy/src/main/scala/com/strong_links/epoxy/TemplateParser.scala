package com.strong_links.epoxy

import com.strong_links.core._
import com.strong_links.core.lex._

import java.io.File

class TemplateParser(file: File) extends LexParser(IO.loadUtf8TextFile(file)) {

  val HtmlStartComment = specialSymbol("<!--")
  val HtmlEndComment = specialSymbol("-->")
  val Def, End, PreserveSpaces = idSymbol
  val Js, Xml, Raw, Field, Label, Control, Help, Error = idSymbol

  class TemplateArgumentMember(val name: String, val firstUseLine: Int, val baseType: String)

  class TemplateArgument(val name: String) {
    var firstUseLine = -1
    var isObject = false
    val members = scala.collection.mutable.Map[String, TemplateArgumentMember]()
    lazy val finalMembers = members.values.toList.sortWith(_.name < _.name)

    def makeType(templateName: String) = {
      if (isObject)
        templateName.capitalize + "_" + name.capitalize
      else
        members("").baseType
    }
  }

  class Template(val templateName: String, val lineNumber: Int, val arguments: List[TemplateArgument],
    val preserveSpace: Boolean, val startPos: Int, val data: String) {
    var code: String = ""
    val body = new LeveledCharStream
    var firstVerbatim = true
    val firstVerbatimPos = startPos
    var verbatimPos = startPos

    def massage(s: String, keepInitialSpace: Boolean, keepTrailingSpace: Boolean) = {
      val addSpaceLeft = keepInitialSpace && (s.length > 0) && s(0).isSpaceChar
      val addSpaceRight = keepTrailingSpace && (s.length > 0) && s(s.length - 1).isSpaceChar
      val w = s.trim
      var r = (addSpaceLeft, addSpaceRight) match {
        case (false, false) => w
        case (false, true) => w + " "
        case (true, false) => " " + w
        case (true, true) => " " + w + " "
      }
      if (r == "  ")
        r = " "
      var done = false
      while (!done) {
        val pos = r.indexOf(HtmlStartComment.special)
        done = pos == -1
        if (!done) {
          val z = r.indexOf(HtmlEndComment.special, pos + HtmlStartComment.special.length)
          if (z == -1)
            Errors.fatal("Unclosed HTML comment.")
          r = r.substring(0, pos) + r.substring(z + HtmlEndComment.special.length)
        }
      }
      r
    }

    def verbatim(endPos: Int, endingTemplate: Boolean) {
      val keepInitialSpace = if (verbatimPos == firstVerbatimPos) preserveSpace else true
      val keepTrailingSpace = if (endingTemplate) preserveSpace else true
      val x = data.substring(verbatimPos, endPos)
      val s = Convert.toScala(massage(x, keepInitialSpace, keepTrailingSpace))
      if (!s.isEmpty)
        body.println("os.write(\"" + s + "\")")
    }

    def makeArgList =
      if (arguments.isEmpty)
        ""
      else
        arguments.map(arg => arg.name + ": " + arg.makeType(templateName)).mkString("(", ", ", ")")

    def generateCode = {
      val isUsingField = arguments.exists(_.finalMembers.exists(_.baseType == T_BASE_FIELD))
      code = IO.usingLeveledCharStream { cs =>
        cs.block("def _1_2(os: OutStream)" << (templateName, makeArgList)) {
          cs.printlnIf(isUsingField, "val ft = fieldTransformer.get")
          cs.println(body.close)
        }
        for (arg <- arguments; if arg.isObject)
          cs.block("type _ =" << arg.makeType(templateName)) {
            arg.finalMembers.foreach(m => cs.println("def _: _" << (m.name, m.baseType)))
          }
      }
    }
  }

  var templates = scala.collection.mutable.Map[String, Template]()
  var currentTemplate: Option[Template] = None

  def startTemplate {
    getToken
    expect(Identifier)
    val declarationLineNumber = token.lineNumber
    val templateName = token.value
    if (!templateName(0).isLower)
      Errors.fatal("The template name _ does not start with a lowercase letter." << templateName)
    if (templates.contains(templateName))
      Errors.fatal("The template _ has already been declared near line _." <<
        (templateName, templates(templateName)))
    getToken
    val args = scala.collection.mutable.ListBuffer[String]()
    expect(LeftParenthesis, HtmlEndComment)
    def getArgument {
      getToken
      expect(Identifier)
      if (token.value(0) != '$')
        Errors.fatal("The argument name _ does not start with a $ sign." << token.value)
      args += token.value.substring(1)
      getToken
    }
    if (token is LeftParenthesis) {
      getArgument
      while (token is Comma)
        getArgument
      skip(RightParenthesis)
    }
    val isPreserveSpace = token is PreserveSpaces
    if (isPreserveSpace)
      getToken
    val posAfterComment = pos
    skip(HtmlEndComment)
    val arguments = args.toList.map(name => new TemplateArgument(name))
    val template = new Template(templateName, declarationLineNumber, arguments, isPreserveSpace, posAfterComment, data)
    currentTemplate = Some(template)
    templates += (templateName -> template)
  }

  def endTemplate(endPos: Int) {
    currentTemplate match {
      case None =>
      case Some(template) =>
        template.verbatim(endPos, true)
        val unusedArguments = template.arguments.filter(_.finalMembers.isEmpty).map(_.name)
        if (!unusedArguments.isEmpty)
          Errors.fatal("Unused arguments: _" << unusedArguments)
        template.generateCode
        currentTemplate = None
    }
  }

  def isArgumentName(arg: String) = {
    arg.startsWith("$") && arg.length > 1 && (arg(1).isLetter || arg(1) == '_')
  }

  def processHtmlComment {
    val savedPos = startPos
    getToken
    if (token in (Def, End)) {
      val start = token is Def
      endTemplate(savedPos)
      if (start)
        startTemplate
    }
  }

  def processReference(arg: TemplateArgument, lineNumber: Int, initialPos: Int, template: Template) {

    template.verbatimPos = initialPos

    // Check if first use, and if so, whether the usage as object/non object is consistent.
    def ono(isObject: Boolean) = if (isObject) "an object" else "a non object"
    val isObject = token is Dot
    val firstUse = arg.firstUseLine == -1
    if (firstUse)
      arg.firstUseLine = lineNumber
    else if (arg.isObject != isObject)
      Errors.fatal("Argument _ is used as _ but it was previously used as _ on line _ (incompatible base types)." <<
        (arg.name, ono(isObject), ono(arg.isObject), arg.firstUseLine))
    arg.isObject = isObject

    // Get the member name. We'll use "" as a dummy member name when the object notation is not used.
    val (memberName, fullMemberName) =
      if (token is Dot) {
        getToken
        expect(Identifier)
        val x = token.value
        if (!x(0).isLower)
          Errors.fatal("Member name _ does not start with a lowercase letter." << x)
        // Position now just after the member name: : $arg.member^
        template.verbatimPos = pos
        getToken
        (x, arg.name + "." + x)
      } else
        ("", arg.name)

    val usage =
      if (token is Colon) {
        getToken
        expect(Js, Raw, Xml, Field, Label, Control, Help, Error)
        val x = token.symbol
        // Position now just after the usage: : $arg:js^ or $arg.member:js^
        template.verbatimPos = pos
        getToken
        x
      } else
        Other

    val (baseType, renderingCode) = usage match {
      case Other => (T_GENERAL_STRING, "os.write(toHtml(_.toString))" << fullMemberName)
      case Js => (T_GENERAL_STRING, "os.write(toJs(_.toString, true))" << fullMemberName)
      case Raw => (T_GENERAL_STRING, "os.write(_.toString)" << fullMemberName)
      case Xml => (T_XML, "os.write(_.toString)" << fullMemberName)
      case Field => (T_BASE_FIELD, "ft.transform(_).render(os)" << fullMemberName)
      case Label => (T_BASE_FIELD, "ft.transform(_).renderLabel(os)" << fullMemberName)
      case Control => (T_BASE_FIELD, "ft.transform(_).renderControl(os)" << fullMemberName)
      case Help => (T_BASE_FIELD, "ft.transform(_).renderHelp(os)" << fullMemberName)
      case Error => (T_BASE_FIELD, "ft.transform(_).renderError(os)" << fullMemberName)
      case _ => Errors.fatal("Invalid usage type _ for _." << (usage, fullMemberName))
    }

    if (arg.members.contains(memberName)) {
      val m = arg.members(memberName)
      if (m.baseType != baseType)
        Errors.fatal("Argument _ is used as _ but it was previously used as _ on line _ (incompatible base types)." <<
          (fullMemberName, baseType, m.baseType, m.firstUseLine))
    } else
      arg.members += (memberName -> new TemplateArgumentMember(memberName, lineNumber, baseType))
    template.body.println(renderingCode)
  }

  def processDollar(dollarPos: Int, lineNumber: Int, template: Template) {
    val argumentName = token.value.substring(1)
    val posAfterArgumentName = pos
    getToken
    template.arguments.find(_.name == argumentName) match {
      case None =>
      case Some(argument) =>
        template.verbatim(dollarPos, false)
        processReference(argument, lineNumber, posAfterArgumentName, template)
    }
  }

  def compile = Errors.liveTrap("_ at line _" << (file, token.lineNumber)) {
    getToken
    while (token isNot Eof) {
      (token.symbol, currentTemplate) match {
        case (HtmlStartComment, _) =>
          processHtmlComment
        case (Identifier, Some(template)) if (isArgumentName(token.value)) =>
          processDollar(startPos, token.lineNumber, template)
        case _ =>
          getToken
      }
    }
    endTemplate(pos)

    templates.values.toList.sortWith(_.templateName < _.templateName)
  }
}
