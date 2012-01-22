package net.strong_links.i18ngen

import net.strong_links.core._
import java.io.File

class I18nKey(val msgCtxt: Option[String], val msgid: String, val msgidPlural: Option[String], emptyMsgidAllowed: Boolean) {

  validate

  private def validate = {
    I18nUtil.validate(msgCtxt, "msgCtxt")
    val ok = emptyMsgidAllowed && msgid.isEmpty
    if (!ok)
      I18nUtil.validate(Some(msgid), "msgid")
    I18nUtil.validate(msgidPlural, "msgid_plural");
  }

  val compute = I18nUtil.compute(msgCtxt, msgid)

  val sortKey = compute.toLowerCase

  lazy val computeForCompiler = I18nUtil.computeForCompiler(msgCtxt, msgid)

  lazy val computeForHuman = I18nUtil.computeForHuman(msgCtxt, msgid)

  override def toString = computeForHuman

  override def equals(that: Any) = that match {
    case x: I18nKey => msgCtxt == x.msgCtxt && msgid == x.msgid
    case _ => false
  }

  override def hashCode = (41 * msgCtxt.hashCode) + msgid.hashCode

  def compare(that: I18nKey): Int = this.sortKey compare that.sortKey
}
