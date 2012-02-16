package com.strong_links.epoxy

import com.strong_links.core._
import com.strong_links.core.lex._

class TemplateFunctionArgument(val templateFunction: TemplateFunction, val name: String) {

  private var _isObject: Option[Boolean] = None
  private var _lineNumber: Option[Int] = None

  def isObject = _isObject match {
    case None     => Errors.fatal("isObject not defined yet.")
    case Some(io) => io
  }

  private val _members = scala.collection.mutable.Map[String, TemplateFunctionArgumentMember]()

  def members = _members.values.toList.sortWith(_.memberName < _.memberName)

  override def toString = "Argument _" <<< name

  def usedAs(isObject: Boolean, lineNumber: Int) {
    (_isObject, _lineNumber) match {
      case (Some(io), Some(ln)) =>
        def ono(x: Boolean) = if (x) "an object" else "a non object"
        if (io != isObject)
          Errors.fatal("Argument _ is used as !_ but it was previously used as !_ on line _ (incompatible base types)." <<
            (name, ono(isObject), ono(io), ln))
      case (None, None) =>
        _isObject = Some(isObject)
        _lineNumber = Some(lineNumber)
      case _ =>
        Errors.badLogic
    }
  }

  def makeType = {
    if (isObject)
      templateFunction.name.capitalize + "_" + name.capitalize
    else
      _members("").getBaseType
  }

  // Search a member and return it. Create if it does not exist.
  def searchMember(optionalMemberName: Option[String]) = {
    val memberName = optionalMemberName.getOrElse("")
    _members.get(memberName) match {
      case None =>
        val x = new TemplateFunctionArgumentMember(this, memberName)
        _members += (memberName -> x)
        x
      case Some(member) =>
        member
    }
  }

  def usesFieldTransformer = _members.values.exists(_.getBaseType == T_BASE_FIELD)
}

