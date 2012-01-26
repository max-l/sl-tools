package com.strong_links

import com.strong_links.core._

package object epoxy {

  type ScannerCreator = EpoxyScanner

  def UUID_SEPARATOR = "++"

  val T_BASE_FIELD = "BaseField[_]"
  val T_GENERAL_STRING = nameOf(classOf[GeneralString])
  val T_XML = nameOf(<a/>.getClass)

  def nameOf(c: Class[_]) = Util.split(c.getName, '.').last
}