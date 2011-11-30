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

  def normalizeName(name: String, context: => LoggingParameter) = {
    def cleanUnderscores(s: String): String = if (!s.contains("__")) s else cleanUnderscores(s.replace("__", "_"))
    val x = cleanUnderscores(Convert.generic(name.toLowerCase, None) {
      case c if c.isLetterOrDigit => null
      case _ => "_"
    })
    if (x.length == 0)
      Errors.fatal("Can't generate a normalized name for _" << name, context)
    if (x(0).isLetter)
      x
    else
      "_" + x
  }
}