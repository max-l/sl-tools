package com.strong_links.epoxy

import com.strong_links.core._
import com.strong_links.core.lex._

class CodeWriter(templateFunction: TemplateFunction, initialStartToken: LexToken) {

  private var flushStartToken: Option[LexToken] = Some(initialStartToken)
  private val out = new LeveledCharStream

  private def massage(s: String, keepInitialSpace: Boolean, keepTrailingSpace: Boolean) = {
    val addSpaceLeft = keepInitialSpace && (s.length > 0) && s(0).isSpaceChar
    val addSpaceRight = keepTrailingSpace && (s.length > 0) && s(s.length - 1).isSpaceChar
    val w = s.trim
    var r = (addSpaceLeft, addSpaceRight) match {
      case (false, false) => w
      case (false, true)  => w + " "
      case (true, false)  => " " + w
      case (true, true)   => " " + w + " "
    }
    if (r == "  ")
      r = " "
    templateFunction.templateCompiler.cleanComments(r)
  }

  def write(s: String) {
    out.println(s)
  }

  def writeStatic(s: String) {
    val x = Convert.toScala(s)
    if (!x.isEmpty)
      write("oc.out.write(\"" + x + "\")")
  }

  def writeMassaged(s: String, keepInitialSpace: Boolean, keepTrailingSpace: Boolean) {
    writeStatic(massage(s, keepInitialSpace, keepTrailingSpace))
  }

  def writeCache[T](data: T, enabled: Boolean)(exp: T => String, formatter: String => String) {
    val callValue = exp(data)
    val s = if (enabled) templateFunction.templateCompiler.cache.store(callValue) else callValue
    write("oc.out.write(_)" << formatter(s))
  }

  def writeI18n(x: String) = if (x != "") {
    writeCache(Convert.toScala(x), templateFunction.cacheI18n)("I18n(\"" + _ + "\")", _ + ".f(oc.i18nLocale)")
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

  def staticFlush(endToken: LexToken, endingTemplate: Boolean) {
    flushStartToken match {
      case None =>
        Errors.fatal("No flush start token is active.")
      case Some(fst) =>
        val x = templateFunction.templateCompiler.getDataBetween(fst, true, endToken, false, true)
        def eval(c: Boolean) = if (c) templateFunction.preserveSpace else true
        writeMassaged(x, eval(flushStartToken eq initialStartToken), eval(endingTemplate))
        flushStartToken = None
    }
  }

  def generateCode = {
    def makeArgList = if (templateFunction.arguments.isEmpty)
      ""
    else
      templateFunction.arguments.map(arg => arg.name + ": " + arg.makeType).mkString("(", ", ", ")")

    val isUsingField = templateFunction.usesFieldTransformer

    IO.usingLeveledCharStream { cs =>

      cs.block("def _1_2(implicit oc: OutputContext)" << (templateFunction.name, makeArgList)) {
        cs.printlnIf(isUsingField, "val ft = fieldTransformer.get")
        cs.println(out.close)
      }

      for (arg <- templateFunction.arguments; if arg.isObject)
        cs.block("type _ =" << arg.makeType) {
          arg.members.foreach(m => cs.println("def _: _" << (m.memberName, m.getBaseType)))
        }
    }
  }
}

