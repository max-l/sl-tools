package net.strong_links.i18ngen

import net.strong_links.core._

import java.io.File

class I18nReference(val file: File, val lineNumber: Int) extends Ordered[I18nReference] {

  override def toString = "File _, line _" << (file.path, lineNumber)

  def compare(that: I18nReference) = {
    val x = this.file.path compare that.file.path
    if (x == 0)
      this.lineNumber compare that.lineNumber
    else
      x
  }

  def asPoComment: String =
    "#: _:_\n" << (file.path, lineNumber)
}
