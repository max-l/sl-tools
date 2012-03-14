package com.strong_links.epoxy

import com.strong_links.core._
import com.strong_links.core.lex._

class CodeWriter(templateFunction: TemplateFunction, initialStartToken: LexToken) {

  private var flushStartToken: Option[LexToken] = Some(initialStartToken)
  private val out = new LeveledCharStream

  def write(s: String) {
    out.println(s)
  }

  def writeStatic(s: String) {
    val x = Convert.toScala(s)
    if (!x.isEmpty)
      write("oc.out.write(\"" + x + "\")")
  }

  def writeMassaged(s: String) {
    writeStatic(templateFunction.massage(s))
  }

  def writeCache[T](data: T, enabled: Boolean)(exp: T => String, formatter: String => String) {
    val callValue = exp(data)
    val s = if (enabled) templateFunction.templateCompiler.cache.store(callValue) else callValue
    write("oc.out.write(_)" << formatter(s))
  }

  def writeI18n(x: String) = if (x != "") {
    writeCache(Convert.toScala(x), templateFunction.cacheI18n)("i18n(\"" + _ + "\")", _ + ".f(oc.i18nLocale)")
  }

  def writeUri(uri: String) {
    val s = Util.split(uri, '/')
    if (s.length < 2)
      Errors.fatal("Invalid Uri _." << uri)
    if (s.head.endsWith(".resources")) {
      val module = s.init.mkString(".")
      val method = s.last
      writeCache((module, method), templateFunction.cacheUri)(x => "_._" << (x._1, x._2), _ + ".format(oc)")

    } else {
      if (s.length != 2)
        Errors.fatal("Interaction Uri _ cannot have parameters." << uri)
      writeCache((s(0), s(1)), templateFunction.cacheUri)(x => "_.uriFor(__._)" << (x._1, x._2), _ + ".format(oc)")
    }
  }

  def staticRestartAt(restartToken: LexToken) {
    flushStartToken = Some(restartToken)
  }

  def staticFlush(endToken: LexToken) {
    flushStartToken match {
      case None =>
        Errors.fatal("No flush start token is active.")
      case Some(fst) =>
        val x = templateFunction.templateCompiler.getDataBetween(fst, true, endToken, false, true)
        writeMassaged(x)
        flushStartToken = None
    }
  }

  def pushCase(templateFunctionArgumentMember: TemplateFunctionArgumentMember) {
    import templateFunctionArgumentMember._
    write("_ match { case None => case Some(_) =>" << (fullMemberName, fullOptionalMemberName))
    out.increaseLevel
  }

  def popCase {
    out.decreaseLevel
    write("}")
  }

  def generateCode = {
    def makeArgList = if (templateFunction.arguments.isEmpty)
      ""
    else
      templateFunction.arguments.map(arg => arg.name + ": " + arg.makeType).mkString("(", ", ", ")")

    IO.usingLeveledCharStream { cs =>

      cs.block("def _1_2 = new TemplateFunction" << (templateFunction.name, makeArgList)) {
        cs.block("def emit(implicit oc: OutputContext) =") {
          cs.println(out.close)
        }
      }

      for (arg <- templateFunction.arguments; if arg.isObject)
        cs.block("type _ =" << arg.makeType) {
          arg.members.foreach(m => cs.println("def _: _" << (m.memberName, m.getFinalType)))
        }
    }
  }
}

