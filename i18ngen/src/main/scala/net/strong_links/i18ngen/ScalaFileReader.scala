package net.strong_links.i18ngen

import net.strong_links.core._

import java.io.File

class ScalaFileReader(file: File, entryBag: PoEntryBag) extends LexParser(IO.loadUtf8TextFile(file)) {
  
  override def getFileName = Some(file.getAbsolutePath)

  import LexSymbol._
  
  // Treat verbatim strings as normal strings.
  override def eatString = {
   if (symbol == verbatimString)
     symbol = string
   super.eatString
  }
  
  // Constants
  val I18nNeutralId = "I18n"
  val I18nNonNeutralId = "I18nPlural"
  val I18nNeutralCtxtId = "I18nCtxt"
  val I18nNonNeutralCtxtId = "I18nPluralCtxt"

  // Comments accumulated in the file. We ignore comments that are not within 5 lines of each other.
  val comments = new FlushableCommentBag(5)

  var entryStartLineNumber = 0
  
  def add(withContext: Boolean, withPlural: Boolean) {
    val msgCtxt = if (withContext) { val ctx = eatString; skip(comma); Some(ctx) } else None
    val msgidValue = eatString
    val msgPlural = if (withPlural) { skip(comma); Some(eatString) } else None
    val reference = new PoReference(file.getAbsolutePath, entryStartLineNumber)
    val e = new PoI18nCall(comments.obtainAtLine(startLineNumber), Some(reference), msgCtxt, msgidValue, msgPlural)
    entryBag.merge(e)
  }

  // We assume that it is a real I18n usage when the identified I18n symbol is followed by a right
  // parenthesis and a literal string. Else we simply ignore.
  def tryAdd(withContext: Boolean, withPlural: Boolean) {
    entryStartLineNumber = startLineNumber
    getSymbol
    if (symbol == leftParenthesis) {
      getSymbol
      if (symbol == string || symbol == verbatimString) 
        add(withContext, withPlural) 
    }
  }
  
  def scan {
    getSymbol
    while (symbol != eof) {
      symbol match {
        case `scalaLineComments` if symbolValue.startsWith("///") =>
          comments.addAtLine(new ScalaPoComment(symbolValue.substring(3)), startLineNumber)
          getSymbol
        case `identifier` if (symbolValue == I18nNeutralId) =>
          tryAdd(false, false)
        case `identifier` if (symbolValue == I18nNeutralCtxtId) =>
          tryAdd(true, false)
        case `identifier` if (symbolValue == I18nNonNeutralId) =>
          tryAdd(false, true)
        case `identifier` if (symbolValue == I18nNonNeutralCtxtId) =>
          tryAdd(true, true)
        case _ =>
          getSymbol
      }
    }
  }
}
