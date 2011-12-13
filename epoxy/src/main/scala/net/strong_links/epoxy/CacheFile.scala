package net.strong_links.epoxy

import net.strong_links.core._

import java.io.File

object FileInfo {

  def toFullName(name: String, uuid: String) = {
    name + UUID_SEPARATOR + uuid
  }

  def apply(file: File, line: String, lineNumber: Int) = {
    def error(what: LoggingParameter*) = {
      val e = "Invalid data found in file _, line _" << (file.getCanonicalPath, lineNumber)
      Errors.fatal(List(e: LoggingParameter) ::: what.toList: _*)
    }
    val segments = Util.split(line, "\t")
    if (segments.length != 3)
      error("_ segments found, but 3 were expected" << segments.length)
    val lastModified = try segments(2).toLong catch {
      case e: Exception => error("Conversion error on _" << segments(2), e)
    }
    new FileInfo(segments(0), segments(1), lastModified)
  }

  def apply(file: File) = {
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

  def makeFunctionName(directory: File) = Errors.context("File _ located in directory _." << (name, directory.getCanonicalPath)) {
    Lex.normalizeName(name)
  }
}

