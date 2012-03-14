package com.strong_links

import com.strong_links.core._

package object epoxy {

  type ScannerCreator = EpoxyScanner

  def UUID_SEPARATOR = "++"

  val T_FORM_FIELD = "FormField[_]"
  val T_STRING = nameOf(classOf[String])
  val T_I18N = nameOf(classOf[I18n])
  val T_XML = nameOf(<a/>.getClass)
  val T_URI = "Uri"
  val T_TEMPLATE = "TemplateFunction"

  def nameOf(c: Class[_]) = Util.split(c.getName, '.').last
}