package com.strong_links.epoxy

import com.strong_links.core._

import java.io.File

object FileInfo {

  def toFullName(name: String, uuid: String) = {
    name + UUID_SEPARATOR + uuid
  }

  def apply(file: File, line: String, lineNumber: Int) = {
    def error(what: LoggingParameter*) = {
      val e = "Invalid data found in _, line _" << (file, lineNumber)
      Errors.fatal(List(e: LoggingParameter) ::: what.toList: _*)
    }
    val segments = Util.nsplit(line, 3, '\t')
    val lastModified = Errors.trap("Conversion error on _" << segments(2))(segments(2).toLong)
    new FileInfo(segments(0), segments(1), lastModified)
  }

  def apply(file: File) = {
    println("Computing md5 of _" << file)
    new FileInfo(file.getName, IO.fileUuid(file), file.lastModified)
  }
}

class FileInfo(val name: String, val uuid: String, val lastModified: Long) {

  import FileInfo._

  override def toString = {
    "_ / _" << (nameUuid, lastModified)
  }

  def nameUuid = {
    toFullName(name, uuid)
  }

  def makeFunctionName(directory: File) = Errors.trap("_ located in _." << (name, directory)) {
    def cleanUnderscores(s: String): String = if (!s.contains("__")) s else cleanUnderscores(s.replace("__", "_"))
    val x = cleanUnderscores(Convert.generic(name.toLowerCase, None) {
      case c if c.isLetterOrDigit => null
      case _ => "_"
    })
    if (x.length == 0)
      Errors.fatal("Can't generate a normalized name for _" << name)
    if (x(0).isLetter)
      x
    else
      "_" + x
  }
}

