package net.strong_links

import net.strong_links.core._

package object epoxy {

  type ScannerCreator = Xlogger => EpoxyScanner

  def UUID_SEPARATOR = "++"

  val T_BASE_FIELD = "BaseField[_]"
  val T_GENERAL_STRING = nameOf(classOf[GeneralString])
  val T_XML = nameOf(<a/>.getClass)

  def nameOf(c: Class[_]) = {
    val segments = c.getName.split('.')
    if (segments.isEmpty)
      Errors.fatal("No dot found in _." << c.getName)
    segments.last
  }
}