package com.strong_links.epoxy

import com.strong_links.core._
import com.strong_links.core.lex._

import java.io.File

class TemplateParser(file: File) extends LexParser(IO.loadUtf8TextFile(file)) {

  val HtmlStartComment = specialSymbol("<!--")
  val HtmlEndComment = specialSymbol("-->")
  val Def, End, Interpretation, On, Off, PreserveSpaces, EnableI18nCache, EnableUriCache = idSymbol
  val String = idSymbol("String")
  val I18n_ = idSymbol("I18n")
  val I18nJs = idSymbol("I18nJs")
  val Js = idSymbol("Js")
  val Xml = idSymbol("Xml")
  val Raw = idSymbol("Raw")
  val Field = idSymbol("Field")
  val Control = idSymbol("Control")
  val Label = idSymbol("Label")
  val Help = idSymbol("Help")
  val Error = idSymbol("Error")
  val Uri = idSymbol("Uri")
  val HardCodedI18n = symbol
  val HardCodedUri = symbol
  val If, Endif = idSymbol

  var interpretation = true
  var enableI18nCache = false
  var enableUriCache = false
  var cacheSequence = 0
  val cache = new LeveledCharStream

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
    var ifLevel = 0

    def processIf(templateArgument: TemplateArgument) {
      println("*** IF _" << ifLevel)
      body.println("if (true) {")
      body.increaseLevel
      ifLevel += 1
    }

    def processEndif {
      println("*** ENDIF _" << ifLevel)
      ifLevel -= 1
      if (ifLevel < 0)
        Errors.fatal("If level dropped below 0 (now _)." << ifLevel)
      body.decreaseLevel
      body.println("}")
    }

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

    def bodyWrite(s: String) {
      val x = Convert.toScala(s)
      if (!x.isEmpty)
        body.println("oc.out.write(\"" + x + "\")")
    }

    def bodyWriteCache[T](data: T, enabled: Boolean)(exp: T => String, formatter: String => String) {
      val callValue = exp(data)
      val out =
        if (enabled) {
          val varName = "__cache" + cacheSequence
          cacheSequence += 1
          cache.println("val _ = _" << (varName, callValue))
          varName
        } else
          callValue
      body.println("oc.out.write(_)" << formatter(out))
    }

    def bodyWriteI18n(x: String) = if (x != "") {
      bodyWriteCache(Convert.toScala(x), enableI18nCache)("I18n(\"" + _ + "\")", _ + ".f(oc.i18nLocale)")
    }

    def bodyWriteUri(uri: String) {
      val s = Util.split(uri, '/')
      if (s.length < 2)
        Errors.fatal("Invalid Uri _." << uri)
      if (s.head.endsWith(".resources")) {
        val module = s.init.mkString(".")
        val method = s.last
        bodyWriteCache((module, method), enableUriCache)(x => "_._" << (x._1, x._2), _ + ".format(oc)")

      } else {
        if (s.length != 2)
          Errors.fatal("Interaction Uri _ cannot have parameters." << uri)
        bodyWriteCache((s(0), s(1)), enableUriCache)(x => "_.uriFor(__._)" << (x._1, x._2), _ + ".format(oc)")
      }
    }

    def verbatim(endPos: Int, restartPos: Int, endingTemplate: Boolean): Unit = {
      val keepInitialSpace = if (verbatimPos == firstVerbatimPos) preserveSpace else true
      val keepTrailingSpace = if (endingTemplate) preserveSpace else true
      val x = data.substring(verbatimPos, endPos)
      bodyWrite(massage(x, keepInitialSpace, keepTrailingSpace))
      verbatimPos = restartPos
    }

    def verbatim(endPos: Int, endingTemplate: Boolean): Unit = verbatim(endPos, verbatimPos, endingTemplate)

    def makeArgList =
      if (arguments.isEmpty)
        ""
      else
        arguments.map(arg => arg.name + ": " + arg.makeType(templateName)).mkString("(", ", ", ")")

    def generateCode = {
      if (ifLevel != 0)
        Errors.fatal("If level not 0.")
      val isUsingField = arguments.exists(_.finalMembers.exists(_.baseType == T_BASE_FIELD))
      code = IO.usingLeveledCharStream { cs =>
        cs.block("def _1_2(implicit oc: OutputContext)" << (templateName, makeArgList)) {
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

  def getOnOff = {
    getToken
    expect(On, Off)
    val r = token is On
    getToken
    r
  }

  def processGlobalHtmlComment {
    val savedPos = startPos
    getToken
    (token.symbol, currentTemplate) match {
      case (Interpretation, _) =>
        interpretation = getOnOff
      case (EnableI18nCache, _) =>
        enableI18nCache = getOnOff
      case (EnableUriCache, _) =>
        enableUriCache = getOnOff
      case (Def | End, _) =>
        val start = token is Def
        endTemplate(savedPos)
        if (start)
          startTemplate
      case (If, Some(template)) =>
        getToken
        expect(Identifier)
        if (!isArgumentName(token.value))
          Errors.fatal("Invalid if argument name _." << token.value)
        val argumentName = token.value.substring(1)
        template.arguments.find(_.name == argumentName) match {
          case None =>
            Errors.fatal("Unknown if argument name _." << argumentName)
          case Some(argument) =>
            template.processIf(argument)
        }
        getToken
      case (Endif, Some(template)) =>
        template.processEndif
        getToken
      case _ =>
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
        expect(String, I18n_, I18nJs, Js, Raw, Xml, Field, Label, Control, Help, Error, Uri)
        val x = token.symbol
        // Position now just after the usage: : $arg:js^ or $arg.member:js^
        template.verbatimPos = pos
        getToken
        x
      } else
        String

    val (baseType, renderingCode) = usage match {
      case String => (T_STRING, "oc.out.write(toHtml(_))" << fullMemberName)
      case I18n_ => (T_I18N, "oc.out.write(toHtml(_.f(oc.i18nLocale)))" << fullMemberName)
      case Js => (T_STRING, "oc.out.write(toJs(_, true))" << fullMemberName)
      case I18nJs => (T_I18N, "oc.out.write(toJs(_.f(oc.i18nLocale), true))" << fullMemberName)
      case Raw => (T_STRING, "oc.out.write(_)" << fullMemberName)
      case Xml => (T_XML, "oc.out.write(_.toString)" << fullMemberName)
      case Field => (T_BASE_FIELD, "ft.transform(_).render(oc)" << fullMemberName)
      case Label => (T_BASE_FIELD, "ft.transform(_).renderLabel(oc)" << fullMemberName)
      case Control => (T_BASE_FIELD, "ft.transform(_).renderControl(oc)" << fullMemberName)
      case Help => (T_BASE_FIELD, "ft.transform(_).renderHelp(oc)" << fullMemberName)
      case Error => (T_BASE_FIELD, "ft.transform(_).renderError(oc)" << fullMemberName)
      case Uri => (T_URI, "oc.out.write(toHtml(_.format(oc)))" << fullMemberName)
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

  def handleHardStringToken(del: Char, symbol: LexSymbol) = {
    getQuoted(del, true, symbol)
    val startPos = token.pos
    currentTemplate match {
      case None =>
      case Some(template) =>
        template.verbatim(startPos, pos, false)
        token.symbol match {
          case HardCodedI18n =>
            template.bodyWriteI18n(token.value)
          case HardCodedUri =>
            template.bodyWriteUri(token.value)
          case other => Errors.badValue(other)
        }
    }
  }

  override def getMiscellaneous {
    if (currentChar == '{' && interpretation)
      handleHardStringToken('}', HardCodedI18n)
    else if (currentChar == '[' && interpretation)
      handleHardStringToken(']', HardCodedUri)
    else
      super.getMiscellaneous
  }

  def compile = Errors.liveTrap("_ at line _" << (file, token.lineNumber)) {
    getToken
    while (token isNot Eof) {
      (token.symbol, currentTemplate) match {
        case (HtmlStartComment, _) =>
          processGlobalHtmlComment
        case (Identifier, Some(template)) if (isArgumentName(token.value)) =>
          processDollar(startPos, token.lineNumber, template)
        case _ =>
          getToken
      }
    }
    endTemplate(pos)

    (templates.values.toList.sortWith(_.templateName < _.templateName), cache.close)
  }
}
