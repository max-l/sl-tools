package com.strong_links.epoxy

import com.strong_links.core._
import com.strong_links.core.lex._

class TemplateFunctionArgumentMember(val templateFunctionArgument: TemplateFunctionArgument, val memberName: String) {

  val tc = templateFunctionArgument.templateFunction.templateCompiler

  if (memberName.length > 0 && !memberName(0).isLower)
    Errors.fatal("Member name _ does not start with a lowercase letter." << memberName)

  val fullMemberName = templateFunctionArgument.name + (if (memberName.length == 0) "" else "." + memberName)

  val fullExternalMemberName = "$" + fullMemberName

  val fullOptionalMemberName = "__" + fullMemberName.replace(".", "_")

  private var _baseTypeLineNumber: Option[Int] = None
  private var _baseType: Option[String] = None

  private var _optionLineNumber: Option[Int] = None
  private var _isOption: Option[Boolean] = None

  def hasBaseType = _baseType != None

  def isBaseField = _baseType match {
    case None     => Errors.fatal("_baseType still undefined.")
    case Some(bt) => bt == T_BASE_FIELD
  }

  def getFinalType: String = (_baseType, _isOption) match {
    case (Some(bt), Some(io)) => if (io) "Option[_]" << bt else bt
    case _                    => Errors.fatal("_baseType or _isOption still undefined.")
  }

  override def toString = "Member _, base type _" << (fullMemberName, _baseType)

  def usedAs(usage: LexSymbol, lineNumber: Int) {
    val baseType = tc.getBaseTypeFor(usage)
    (_baseType, _baseTypeLineNumber) match {
      case (None, None) =>
        _baseType = Some(baseType)
        _baseTypeLineNumber = Some(lineNumber)
      case (Some(bt), Some(btln)) =>
        if (bt != baseType)
          Errors.fatal("Argument _ is used as _ but it was previously used as _ on line _ (incompatible base types)." <<
            (fullMemberName, baseType, bt, btln))
      case _ =>
        Errors.badLogic
    }
  }

  def optionAs(isOption: Boolean, lineNumber: Int) {
    def ono(x: Boolean) = if (x) "optional" else "non optional"
    (_isOption, _optionLineNumber) match {
      case (None, None) =>
        _isOption = Some(isOption)
        _optionLineNumber = Some(lineNumber)
      case (Some(io), Some(oln)) =>
        if (io != isOption)
          Errors.fatal("Argument member_ is used as _ but it was previously used as _ on line _ (incompatible base types)." <<
            (fullMemberName, ono(isOption), ono(io), oln))
      case _ =>
        Errors.badLogic
    }
  }

  def render(usage: LexSymbol) = {
    _isOption match {
      case None =>
        Errors.fatal("_isOption is still undefined.")
      case Some(isOption) =>
        tc.getRenderCodeFor(usage, if (isOption) fullOptionalMemberName else fullMemberName)
    }
  }
}

