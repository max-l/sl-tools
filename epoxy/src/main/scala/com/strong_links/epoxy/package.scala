package com.strong_links

import com.strong_links.core._

package object epoxy {

  type ScannerCreator = EpoxyScanner

  def UUID_SEPARATOR = "++"

  val T_BASE_FIELD = "BaseField[_]"
  val T_STRING = nameOf(classOf[String])
  val T_I18N = nameOf(classOf[I18n])
  val T_XML = nameOf(<a/>.getClass)
  val T_URI = "Uri"

  def nameOf(c: Class[_]) = Util.split(c.getName, '.').last
}