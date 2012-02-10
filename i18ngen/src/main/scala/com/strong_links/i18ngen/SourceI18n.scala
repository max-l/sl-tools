package com.strong_links.i18ngen

import com.strong_links.core._
import java.io.File

class SourceI18nCall(val packageSegments: List[String], msgCtxt: Option[String], msgid: String, msgidPlural: Option[String], val comments: List[ScalaComment],
  file: File, lineNumber: Int) {

  I18nConfig.checkPackageSegments(packageSegments)

  val key = new I18nKey(msgCtxt, msgid, msgidPlural, false)

  val reference = new I18nReference(file, lineNumber)

  override def toString = "!_ in package _, referenced at !_" <<< (key, packageSegments.mkString("."), reference)
}

class SourceI18nCallSummary(msgCtxt: Option[String], msgid: String, msgidPlural: Option[String], val comments: List[ScalaComment],
  _references: List[I18nReference]) {
  val key = new I18nKey(msgCtxt, msgid, msgidPlural, false)
  def references = _references.sorted
}
