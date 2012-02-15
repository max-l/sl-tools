package com.strong_links.epoxy

import com.strong_links.core._
import com.strong_links.core.lex._

class FunctionArgumentMember(val templateFunctionArgument: TemplateFunctionArgument,
                             val memberName: String, val fullMemberName: String, val firstUseLineNumber: Int, val baseType: String) {

  override def toString = "Member _, full name _, base type _" << (memberName, fullMemberName, baseType)

  def render(usage: LexSymbol) = templateFunctionArgument.templateFunction.templateCompiler.getRenderCodeFor(usage, fullMemberName)
}

