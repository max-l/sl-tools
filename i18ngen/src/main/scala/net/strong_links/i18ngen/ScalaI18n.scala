package net.strong_links.i18ngen

import net.strong_links.core._
import java.io.File

class ScalaI18nCall(msgCtxt: Option[String], msgid: String, msgidPlural: Option[String], val comments: List[ScalaComment],
  file: File, lineNumber: Int) {

  val key = new I18nKey(msgCtxt, msgid, msgidPlural, false)

  val reference = new I18nReference(file, lineNumber)
}

class ScalaI18nCallSummary(msgCtxt: Option[String], msgid: String, msgidPlural: Option[String], val comments: List[ScalaComment],
  val references: List[I18nReference]) {
  val key = new I18nKey(msgCtxt, msgid, msgidPlural, false)
}
