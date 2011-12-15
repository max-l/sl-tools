package net.strong_links.epoxy

import net.strong_links.core._

import java.io.File

class TemplateParser(file: File) extends BasicLexParser(IO.loadUtf8TextFile(file)) {

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
      val commentStart = "<!--"
      val commentEnd = "-->"
      var done = false
      while (!done) {
        val pos = r.indexOf(commentStart)
        done = pos == -1
        if (!done) {
          val z = r.indexOf(commentEnd, pos + commentStart.length)
          if (z == -1)
            Errors.fatal("Unclosed HTML comment.")
          r = r.substring(0, pos) + r.substring(z + commentEnd.length)
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

  import LexSymbol._

  override def getSymbolHere {
    if (currentChar == '<' && nextChar == '!' && nextNextChar == '-' && nextNextNextChar == '-') {
      move(4)
      symbol = htmlStartComment
    } else if (currentChar == '-' && nextChar == '-' && nextNextChar == '>') {
      move(3)
      symbol = htmlEndComment
    } else
      super.getSymbolHere
  }

  var templates = scala.collection.mutable.Map[String, Template]()
  var currentTemplate: Option[Template] = None

  def templateStart {
    getSymbol
    expect(identifier)
    val declarationLineNumber = startLineNumber
    val templateName = symbolValue
    if (!templateName(0).isLower)
      Errors.fatal("The template name _ does not start with a lowercase letter." << templateName)
    if (templates.contains(templateName))
      Errors.fatal("The template _ has already been declared near line _." <<
        (templateName, templates(templateName)))
    getSymbol
    val args = scala.collection.mutable.ListBuffer[String]()
    expect(Set(leftParenthesis, htmlEndComment))
    def getArgument {
      getSymbol
      expect(identifier)
      if (symbolValue(0) != '$')
        Errors.fatal("The argument name _ does not start with a $ sign." << symbolValue)
      args += symbolValue.substring(1)
      getSymbol
    }
    if (symbol == leftParenthesis) {
      getArgument
      while (symbol == comma)
        getArgument
      skip(rightParenthesis)
    }
    val isPreserveSpace = (symbol == preserveSpaces)
    if (isPreserveSpace)
      getSymbol
    val posAfterComment = pos
    skip(htmlEndComment)
    val arguments = args.toList.map(name => new TemplateArgument(name))
    val template = new Template(templateName, declarationLineNumber, arguments, isPreserveSpace, posAfterComment, data)
    currentTemplate = Some(template)
    templates += (templateName -> template)
  }

  def templateEnd(endPos: Int) {
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
    arg.startsWith("$") && arg.length > 1 && isIdentifierCharacter(arg(1))
  }

  def processHtmlComment {
    val savedPos = startPos
    getSymbol
    if (symbol == template || symbol == LexSymbol.templateEnd) {
      val start = symbol == template
      templateEnd(savedPos)
      if (start)
        templateStart
    }
  }

  def processReference(arg: TemplateArgument, lineNumber: Int, initialPos: Int, template: Template) {

    template.verbatimPos = initialPos

    // Check if first use, and if so, whether the usage as object/non object is consistent.
    def ono(isObject: Boolean) = if (isObject) "an object" else "a non object"
    val isObject = (symbol == dot)
    val firstUse = arg.firstUseLine == -1
    if (firstUse)
      arg.firstUseLine = lineNumber
    else if (arg.isObject != isObject)
      Errors.fatal("Argument _ is used as _ but it was previously used as _ on line _ (incompatible base types)." <<
        (arg.name, ono(isObject), ono(arg.isObject), arg.firstUseLine))
    arg.isObject = isObject

    // Get the member name. We'll use "" as a dummy member name when the object notation is not used.
    val (memberName, fullMemberName) =
      if (symbol == dot) {
        getSymbol
        expect(identifier)
        val x = symbolValue
        if (!x(0).isLower)
          Errors.fatal("Member name _ does not start with a lowercase letter." << x)
        // Position now just after the member name: : $arg.member^
        template.verbatimPos = pos
        getSymbol
        (x, arg.name + "." + x)
      } else
        ("", arg.name)

    val usage =
      if (symbol == colon) {
        getSymbol
        expect(identifier)
        val x = symbolValue
        // Position now just after the usage: : $arg:js^ or $arg.member:js^
        template.verbatimPos = pos
        getSymbol
        x
      } else
        ""

    val (baseType, renderingCode) = usage match {
      case "" => (T_GENERAL_STRING, "os.write(toHtml(_.toString))" << fullMemberName)
      case "js" => (T_GENERAL_STRING, "os.write(toJs(_.toString, true))" << fullMemberName)
      case "raw" => (T_GENERAL_STRING, "os.write(_.toString)" << fullMemberName)
      case "xml" => (T_XML, "os.write(_.toString)" << fullMemberName)
      case "field" => (T_BASE_FIELD, "ft.transform(_).render(os)" << fullMemberName)
      case "fieldLabel" => (T_BASE_FIELD, "ft.transform(_).renderLabel(os)" << fullMemberName)
      case "fieldControl" => (T_BASE_FIELD, "ft.transform(_).renderControl(os)" << fullMemberName)
      case "fieldHelp" => (T_BASE_FIELD, "ft.transform(_).renderHelp(os)" << fullMemberName)
      case "fieldError" => (T_BASE_FIELD, "ft.transform(_).renderError(os)" << fullMemberName)
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
    val argumentName = symbolValue.substring(1)
    val posAfterArgumentName = pos
    getSymbol
    template.arguments.find(_.name == argumentName) match {
      case None =>
      case Some(argument) =>
        template.verbatim(dollarPos, false)
        processReference(argument, lineNumber, posAfterArgumentName, template)
    }
  }

  def compile = Errors.trap("Template file _" << file) {
    getSymbol
    while (symbol != eof)
      (symbol, currentTemplate) match {
        case (`htmlStartComment`, _) =>
          processHtmlComment
        case (`identifier`, Some(template)) if (isArgumentName(symbolValue)) =>
          processDollar(startPos, startLineNumber, template)
        case _ =>
          getSymbol
      }
    templateEnd(pos)

    templates.values.toList.sortWith(_.templateName < _.templateName)
  }
}
