package com.strong_links.epoxy

import com.strong_links.core._
import com.strong_links.core.lex._

class TemplateFunctionArgument(val templateFunction: TemplateFunction, val name: String) {
  private var _lineNumber = -1
  private var _isObject = false
  def isObject = _isObject
  private val _members = scala.collection.mutable.Map[String, FunctionArgumentMember]()
  def members = _members.values.toList.sortWith(_.memberName < _.memberName)

  override def toString = "Argument _" <<< name

  def usedAs(isObject: Boolean, lineNumber: Int) {
    if (_lineNumber == -1) {
      _lineNumber = lineNumber
      _isObject = isObject
    } else if (_isObject != isObject) {
      def ono(isObject: Boolean) = if (isObject) "an object" else "a non object"
      Errors.fatal("Argument _ is used as !_ but it was previously used as !_ on line _ (incompatible base types)." <<
        (name, ono(isObject), ono(_isObject), _lineNumber))
    }
  }

  def makeType = {
    if (_isObject)
      templateFunction.name.capitalize + "_" + name.capitalize
    else
      _members("").baseType
  }

  def addMember(optionalMemberName: Option[String], usage: LexSymbol, lineNumber: Int) = {
    val (memberName, fullMemberName) = optionalMemberName match {
      case None => ("", name)
      case Some(x) =>
        if (!x(0).isLower)
          Errors.fatal("Member name _ does not start with a lowercase letter." << x)
        (x, name + "." + x)
    }
    val baseType = templateFunction.templateCompiler.getBaseTypeFor(usage)
    if (_members.contains(memberName)) {
      val m = _members(memberName)
      if (m.baseType != baseType)
        Errors.fatal("Argument _ is used as _ but it was previously used as _ on line _ (incompatible base types)." <<
          (fullMemberName, baseType, m.baseType, m.firstUseLineNumber))
    } else {
      _members += (memberName -> new FunctionArgumentMember(this, memberName, fullMemberName, lineNumber, baseType))
    }
    _members(memberName)
  }

  def usesFieldTransformer = _members.values.exists(_.baseType == T_BASE_FIELD)
}

