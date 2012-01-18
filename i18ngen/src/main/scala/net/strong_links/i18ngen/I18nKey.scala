package net.strong_links.i18ngen

import net.strong_links.core._
import java.io.File

object I18nKey {

  def validate(os: Option[String], what: String) = {
    os match {
      case None =>
      case Some(s) =>
        if (s.trim != s)
          Errors.fatal("The _ string _ has leading or trailing whitespace." << (what, s))
        if (s == "")
          Errors.fatal("Empty _ string." << what)
    }
  }

  def compute(msgCtxt: Option[String], msgid: String) = msgCtxt match {
    case None => msgid
    case Some(ctx) => ctx + "\u0000" + msgid
  }

  def computeForHuman(msgCtxt: Option[String], msgid: String): String = msgCtxt match {
    case None => "msgid _" <<< msgid
    case Some(ctx) => "msgctxt _ and msgid _" <<< (ctx, msgid)
  }
}

class I18nKey(val msgCtxt: Option[String], val msgid: String, val msgidPlural: Option[String], emptyMsgidAllowed: Boolean) {

  validate

  val sortKey = compute.toLowerCase

  private def validate = {
    I18nKey.validate(msgCtxt, "msgCtxt")
    val ok = emptyMsgidAllowed && msgid.isEmpty
    if (!ok)
      I18nKey.validate(Some(msgid), "msgid")
    I18nKey.validate(msgidPlural, "msgid_plural");
  }

  def compute = I18nKey.compute(msgCtxt, msgid)

  def computeForHuman = I18nKey.computeForHuman(msgCtxt, msgid)

  override def toString = computeForHuman

  override def equals(any: Any) = any match {
    case that: I18nKey => msgCtxt == that.msgCtxt && msgid == that.msgid
    case _ => false
  }

  override def hashCode = (41 * msgCtxt.hashCode) + msgid.hashCode

  def compare(that: I18nKey) = this.sortKey compare that.sortKey
}
