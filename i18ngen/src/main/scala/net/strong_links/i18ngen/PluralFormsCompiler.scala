package net.strong_links.i18ngen

import net.strong_links.core._
import scala.collection.mutable.ListBuffer

import LexSymbol._

class PluralFormsCompiler(data: String, loggers: Loggers) extends LexParser(data, loggers) {
  val output = ListBuffer[String]()
  var stackLevel = -1
  val allocatedRegisters = scala.collection.mutable.Set[Int]()
  
  def compile = {
    getSymbol
    output += "def computePluralForm(n: Int): Int = {"
    expression
    if (symbol != eof)
      error(startLineNumber, "Expected end of expression.")
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
       case `plus` => binaryOp("_ = _ + _" << (_, _, _))
       case `minus` => binaryOp("_ = _ - _" << (_, _, _))
       case `unaryMinus` => unaryOp("_ = -_" << (_, _))
       case `logicalNot` => unaryOp("_ =  if (_ == 0) 1 else 0" << (_, _))
       case `multiply` => binaryOp("_ =  _ * _" << (_, _, _))
       case `divide` => binaryOp("_ =  _ / _" << (_, _, _))
       case `modulo` => binaryOp("_ =  _ % _" << (_, _, _))
       case `lessThan` => binaryOp("_ =  if (_ < _) 1 else 0" << (_, _, _))
       case `lessThanOrEqual` => binaryOp("_ =  if (_ <= _) 1 else 0" << (_, _, _))
       case `greaterThan` => binaryOp("_ =  if (_ > _) 1 else 0" << (_, _, _))
       case `greaterThanOrEqual` => binaryOp("_ =  if (_ >= _) 1 else 0" << (_, _, _))
       case `equal` => binaryOp("_ =  if (_ == _) 1 else 0" << (_, _, _))
       case `notEqual` => binaryOp("_ =  if (_ != _) 1 else 0" << (_, _, _))
       case `logicalAnd` => binaryOp("_ =  if (_ != 0 && _ != 0) 1 else 0" << (_, _, _))
       case `logicalOr` => binaryOp("_ =  if (_ != 0 || _ != 0) 1 else 0" << (_, _, _))
       case `triadic` => tertiaryOp("_ =  if (_ != 0) _ else _" << (_, _, _, _))
    }
    output += "  _" << statement
  }
  
  def emitReturn {
    if (stackLevel != 0)
      Errors.fatal("Invalid stack.")
    output += "  _" << registerName(stackLevel)
    stackLevel -= 1
  }
  
  def walk(s: Set[LexSymbol])(u: => Unit) {
    u  
    while (symbol in s) { 
      val operator = eatSymbol._1
      u
      op(operator)
    }
  }

  def p0 {
    symbol match {
      case `n` => 
        push("n")
        getSymbol
      case `leftParenthesis` => 
        getSymbol
        expression
        expect(rightParenthesis)
        getSymbol
      case `number` =>
        push(symbolValue)
        getSymbol
      case _ if (symbol in Set(plus, minus, logicalNot)) =>
        val unaryOp = eatSymbol._1
        p0
        if (unaryOp == minus)
          op(unaryMinus)
        else if (unaryOp != plus)
          op(unaryOp)
      case _ =>
        error(startLineNumber, "Invalid symbol '_' to start a factor." << symbol)
    }
  }

  def p1 = walk(Set(multiply, divide, modulo))(p0)
  
  def p2 = walk(Set(plus, minus))(p1)
  
  def p3 = walk(Set(lessThan, lessThanOrEqual, greaterThan, greaterThanOrEqual))(p2)
  
  def p4 = walk(Set(equal, notEqual))(p3)
  
  def p5 = walk(Set(logicalAnd))(p4)
  
  def p6 = walk(Set(logicalOr))(p5)
  
  def p7: Unit = {
    p6
    if (symbol == questionMark) {
      getSymbol
      p6
      expect(colon)
      getSymbol
      p6
      op(triadic)
    }
  }

  def expression = p7
}
