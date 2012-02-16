package com.strong_links.epoxy

import com.strong_links.core._
import com.strong_links.core.lex._

class TemplateFunctionArgumentMember(val templateFunctionArgument: TemplateFunctionArgument, val memberName: String) {

  val tc = templateFunctionArgument.templateFunction.templateCompiler

  if (memberName.length > 0 && !memberName(0).isLower)
    Errors.fatal("Member name _ does not start with a lowercase letter." << memberName)

  def fullMemberName = templateFunctionArgument.name + (if (memberName.length == 0) "" else "." + memberName)

  private var _lineNumber: Option[Int] = None
  private var _baseType: Option[String] = None

  def getBaseType = _baseType match {
    case None     => Errors.fatal("_baseType still undefined.")
    case Some(bt) => bt
  }

  override def toString = "Member _, base type _" << (fullMemberName, _baseType)

  def usedAs(usage: LexSymbol, lineNumber: Int) {
    val baseType = tc.getBaseTypeFor(usage)
    (_baseType, _lineNumber) match {
      case (None, None) =>
        _baseType = Some(baseType)
        _lineNumber = Some(lineNumber)
      case (Some(bt), Some(ln)) =>
        if (bt != baseType)
          Errors.fatal("Argument _ is used as _ but it was previously used as _ on line _ (incompatible base types)." <<
            (fullMemberName, baseType, bt, ln))
      case _ =>
        Errors.badLogic
    }
  }

  def render(usage: LexSymbol) = tc.getRenderCodeFor(usage, fullMemberName)
}

