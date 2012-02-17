package com.strong_links.epoxy

import com.strong_links.core._
import com.strong_links.core.lex._

import java.io.File

class TemplateParser(file: File) extends LexParser(IO.loadUtf8TextFile(file)) {

  val Def, PreserveComments, DisableInterpretation, CacheI18n, CacheUri, End, Ifdef, Endif = idSymbol
  val HtmlStartComment = specialSymbol("<!--")
  val HtmlEndComment = specialSymbol("-->")
  val LeftBrace = specialSymbol("{")
  val RightBrace = specialSymbol("}")
  val LeftBracket = specialSymbol("[")
  val RightBracket = specialSymbol("]")
  val Html = idSymbol("Html")
  val I18n_ = idSymbol("I18n")
  val I18nJs = idSymbol("I18nJs")
  val Js = idSymbol("Js")
  val I18nJsHtml = idSymbol("I18nJsHtml")
  val JsHtml = idSymbol("JsHtml")
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
}
