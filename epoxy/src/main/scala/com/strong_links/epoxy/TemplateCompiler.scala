package com.strong_links.epoxy

import com.strong_links.core._
import com.strong_links.core.lex._
import java.io.File

class TemplateCompiler(file: File) extends TemplateParser(file) {

  val templateFunctions = scala.collection.mutable.Map[String, TemplateFunction]()

  val cache = new Cache

  def getBaseTypeFor(usage: LexSymbol) = usage match {
    case Html | Js | JsHtml | Raw               => T_STRING
    case I18n_ | I18nJs | I18nJsHtml            => T_I18N
    case Xml                                    => T_XML
    case Field | Label | Control | Help | Error => T_BASE_FIELD
    case Uri                                    => T_URI
    case Template                               => T_TEMPLATE
    case _                                      => Errors.badValue(usage)
  }

  def getRenderCodeFor(usage: LexSymbol, fullMemberName: String) = usage match {
    case Html       => "oc.out.write(toHtml(_))" << fullMemberName
    case I18n_      => "oc.out.write(toHtml(_.f(oc.i18nLocale)))" << fullMemberName
    case Js         => "oc.out.write(toJs(_, false))" << fullMemberName
    case I18nJs     => "oc.out.write(toJs(_.f(oc.i18nLocale), false))" << fullMemberName
    case JsHtml     => "oc.out.write(toHtml(toJs(_, false)))" << fullMemberName
    case I18nJsHtml => "oc.out.write(toHtml(toJs(_.f(oc.i18nLocale), false)))" << fullMemberName
    case Raw        => "oc.out.write(_)" << fullMemberName
    case Xml        => "oc.out.write(_.toString)" << fullMemberName
    case Field      => "ft.transform(_).render(oc)" << fullMemberName
    case Label      => "ft.transform(_).renderLabel(oc)" << fullMemberName
    case Control    => "ft.transform(_).renderControl(oc)" << fullMemberName
    case Help       => "ft.transform(_).renderHelp(oc)" << fullMemberName
    case Error      => "ft.transform(_).renderError(oc)" << fullMemberName
    case Uri        => "oc.out.write(toHtml(_.toString))" << fullMemberName
    case Template   => "_.emit(oc)" << fullMemberName
    case _          => Errors.badValue(usage)
  }

  def processDef {
    // We enter here when a "def" is found after an html comment start.
    val templateFunction = new TemplateFunction(this)
    templateFunctions += (templateFunction.name -> templateFunction)
    if (templateFunction.hasNextFunction)
      processDef
  }

  def compile = Errors.liveTrap("_ at line _" << (file, token.lineNumber)) {
    getToken
    while (token isNot Eof)
      if (token is HtmlStartComment) {
        getToken
        if (token is Def)
          processDef
      } else
        getToken

    (templateFunctions.values.toList.sortWith(_.name < _.name), cache.get)
  }
}