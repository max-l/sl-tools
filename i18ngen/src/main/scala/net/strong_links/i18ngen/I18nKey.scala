package net.strong_links.i18ngen

import net.strong_links.core._
import java.io.File

object I18nKey {
  def computeFor(msgCtxt: Option[String], msgid: String) = msgCtxt match {
    case None => msgid
    case Some(ctx) => ctx + "\u0000" + msgid
  }

  def computeForHuman(msgCtxt: Option[String], msgid: String) = msgCtxt match {
    case None => "msgid _" <<< msgid
    case Some(ctx) => "ctx _ and msgid _" <<< (ctx, msgid)
  }

  def group[T <: I18nKey, V](items: List[T])(errorHandler: List[T] => LoggingParameter)(work: (Option[String], String, Option[String], List[T]) => V) =
    for (
      ((msgCtxt, msgid), calls) <- items.groupBy(i => (i.msgCtxt, i.msgid)).toList;
      distinctMsgidPlurals = calls.map(_.msgidPlural).distinct
    ) yield distinctMsgidPlurals match {
      case List(msgidPlural) =>
        work(msgCtxt, msgid, msgidPlural, calls)
      case _ =>
        val msg = ("The I18n key _ has these incompatible plural forms " << I18nKey.computeForHuman(msgCtxt, msgid)) +
          ("_." <<< distinctMsgidPlurals)
        Errors.fatal(msg, errorHandler(calls))
    }
}

class I18nKey(val msgCtxt: Option[String], val msgid: String, val msgidPlural: Option[String], emptyMsgidAllowed: Boolean) {

  protected def validate(os: Option[String], what: String) = {
    os match {
      case None =>
      case Some(s) =>
        if (s.trim != s)
          Errors.fatal("The _ string _ has leading or trailing whitespace." << (what, s))
        if (s == "")
          Errors.fatal("Empty _ string." << what)
    }
  }

  validate(msgCtxt, "msgCtxt")
  val ok = emptyMsgidAllowed && msgid.isEmpty
  if (!ok)
    validate(Some(msgid), "msgid")
  validate(msgidPlural, "msgid_plural");

  override def toString = I18nKey.computeForHuman(msgCtxt, msgid)

  override def equals(any: Any) = any match {
    case that: I18nKey => msgCtxt == that.msgCtxt && msgid == that.msgid
    case _ => false
  }

  override def hashCode = (41 * msgCtxt.hashCode) + msgid.hashCode

  def compare(that: I18nKey): Int = (this.msgCtxt.getOrElse("") compare that.msgCtxt.getOrElse("")) match {
    case 0 => this.msgid compare that.msgid
    case x => x
  }
}
