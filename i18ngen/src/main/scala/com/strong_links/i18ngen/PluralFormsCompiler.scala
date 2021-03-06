package com.strong_links.i18ngen

import com.strong_links.core._
import com.strong_links.core.lex._

class PluralFormsCompiler(data: String) extends LexParser(data) {
  val UnaryMinus = symbol
  val N = idSymbol
  val Plus = specialSymbol("+")
  val Minus = specialSymbol("-")
  val Triadic = specialSymbol(":")
  val LogicalNot = specialSymbol("!")
  val Multiply = specialSymbol("*")
  val Divide = specialSymbol("/")
  val Modulo = specialSymbol("%")
  val LessThan = specialSymbol("<")
  val LessThanOrEqual = specialSymbol("<=")
  val GreaterThan = specialSymbol(">")
  val GreaterThanOrEqual = specialSymbol(">=")
  val Equal = specialSymbol("==")
  val NotEqual = specialSymbol("!=")
  val LogicalAnd = specialSymbol("&&")
  val LogicalOr = specialSymbol("||")
  val QuestionMark = specialSymbol("?")

  val output = scala.collection.mutable.ListBuffer[String]()
  var stackLevel = -1
  val allocatedRegisters = scala.collection.mutable.Set[Int]()

  def compile = Errors.trap("Invalid plural forms expression _" << data) {
    getToken
    output += "def computePluralForm(n: Int): Int = {"
    expression
    expect(Eof)
    emitReturn
    output += "}"
    output.map("  " + _).mkString("\n")
  }

  def push(what: String) {
    stackLevel += 1
    output += "  _ = _" << (registerName(stackLevel), what)
  }

  def registerName(stackLevel: Int) = {
    val prefix =
      if (allocatedRegisters.contains(stackLevel))
        ""
      else {
        allocatedRegisters += stackLevel
        "var "
      }
    prefix + "r" + stackLevel
  }

  def unaryOp(f: (String, String) => String) = {
    if (stackLevel < 0)
      Errors.fatal("Invalid stack.")
    f(registerName(stackLevel), registerName(stackLevel))
  }

  def binaryOp(f: (String, String, String) => String) = {
    if (stackLevel < 1)
      Errors.fatal("Invalid stack.")
    stackLevel -= 1
    f(registerName(stackLevel), registerName(stackLevel), registerName(stackLevel + 1))
  }

  def tertiaryOp(f: (String, String, String, String) => String) = {
    if (stackLevel < 2)
      Errors.fatal("Invalid stack.")
    stackLevel -= 2
    f(registerName(stackLevel), registerName(stackLevel), registerName(stackLevel + 1), registerName(stackLevel + 2))
  }

  def op(operator: LexSymbol) {
    val statement = operator match {
      case Plus => binaryOp("_ = _ + _" << (_, _, _))
      case Minus => binaryOp("_ = _ - _" << (_, _, _))
      case UnaryMinus => unaryOp("_ = -_" << (_, _))
      case LogicalNot => unaryOp("_ =  if (_ == 0) 1 else 0" << (_, _))
      case Multiply => binaryOp("_ =  _ * _" << (_, _, _))
      case Divide => binaryOp("_ =  _ / _" << (_, _, _))
      case Modulo => binaryOp("_ =  _ % _" << (_, _, _))
      case LessThan => binaryOp("_ =  if (_ < _) 1 else 0" << (_, _, _))
      case LessThanOrEqual => binaryOp("_ =  if (_ <= _) 1 else 0" << (_, _, _))
      case GreaterThan => binaryOp("_ =  if (_ > _) 1 else 0" << (_, _, _))
      case GreaterThanOrEqual => binaryOp("_ =  if (_ >= _) 1 else 0" << (_, _, _))
      case Equal => binaryOp("_ =  if (_ == _) 1 else 0" << (_, _, _))
      case NotEqual => binaryOp("_ =  if (_ != _) 1 else 0" << (_, _, _))
      case LogicalAnd => binaryOp("_ =  if (_ != 0 && _ != 0) 1 else 0" << (_, _, _))
      case LogicalOr => binaryOp("_ =  if (_ != 0 || _ != 0) 1 else 0" << (_, _, _))
      case Triadic => tertiaryOp("_ =  if (_ != 0) _ else _" << (_, _, _, _))
    }
    output += "  _" << statement
  }

  def emitReturn {
    if (stackLevel != 0)
      Errors.fatal("Invalid stack.")
    output += "  _" << registerName(stackLevel)
    stackLevel -= 1
  }

  def walk(s: LexSymbol*)(u: => Unit) {
    u
    while (token in (s: _*)) {
      val operator = eatAnyToken.symbol
      u
      op(operator)
    }
  }

  def p0 {
    token.symbol match {
      case N =>
        push("n")
        getToken
      case LeftParenthesis =>
        getToken
        expression
        expect(RightParenthesis)
        getToken
      case Number =>
        push(token.value)
        getToken
      case _ if (token in (Plus, Minus, LogicalNot)) =>
        val unaryOp = eatAnyToken.symbol
        p0
        if (unaryOp == Minus)
          op(UnaryMinus)
        else if (unaryOp != Plus)
          op(unaryOp)
      case _ =>
        Errors.fatal("Invalid token '_' to start a factor." << token)
    }
  }

  def p1 = walk(Multiply, Divide, Modulo)(p0)

  def p2 = walk(Plus, Minus)(p1)

  def p3 = walk(LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual)(p2)

  def p4 = walk(Equal, NotEqual)(p3)

  def p5 = walk(LogicalAnd)(p4)

  def p6 = walk(LogicalOr)(p5)

  def p7 = {
    p6
    if (token is QuestionMark) {
      getToken
      p6
      expect(Colon)
      getToken
      p6
      op(Triadic)
    }
  }

  def expression = p7
}
