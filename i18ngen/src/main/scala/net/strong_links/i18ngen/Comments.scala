package net.strong_links.i18ngen

import net.strong_links.core._

abstract class Comment(_value: String) {
  def value = _value.trim
}

class ScalaComment(value: String) extends Comment(value)

abstract class PoComment(value: String) extends Comment(value)

class TranslatorPoComment(value: String) extends PoComment(value)

class ObsoletePoComment(value: String) extends PoComment(value)

class Comments[T <: Comment] {
  private val contents = scala.collection.mutable.ListBuffer[T]()

  protected def clear {
    contents.clear
  }

  def add(comment: T) {
    if (comment.value != "")
      contents += comment
  }

  def obtainAndClear: List[T] = {
    val list = contents.toList
    clear
    list
  }
}

class FlushableComments[T <: Comment](proximityThreshold: Int) extends Comments[T] {

  private var lastLineNumber: Int = Int.MinValue

  override protected def clear {
    super.clear
    lastLineNumber = Int.MinValue
  }

  private def check(lineNumber: Int) {
    if ((lineNumber - lastLineNumber) > proximityThreshold)
      clear
    lastLineNumber = lineNumber
  }

  def addAtLine(comment: T, lineNumber: Int) {
    check(lineNumber)
    add(comment)
  }

  def obtainAtLine(lineNumber: Int): List[T] = {
    check(lineNumber)
    obtainAndClear
  }
}

class PoComments extends Comments[PoComment] {

  var fuzzy = false

  override protected def clear {
    super.clear
    fuzzy = false
  }

  def obtainWithFuzzyAndClear: (Boolean, List[PoComment]) = {
    (fuzzy, super.obtainAndClear)
  }
}

class ScalaComments(proximityThreshold: Int) extends FlushableComments[ScalaComment](proximityThreshold)
