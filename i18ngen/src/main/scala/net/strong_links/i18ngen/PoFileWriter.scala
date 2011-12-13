package net.strong_links.i18ngen

import net.strong_links.core._

import java.io.File

class PoFileWriter(file: File, nbPluralForms: Int, entries: List[PoEntry], backupComments: List[PoComment]) {

  private val cs = new CharStream

  def generateAndClose {
    var missingTranslations = 0
    for (e <- entries) {
      new PoEntryWriter(e, cs).write
      if (e.translations == Nil)
        missingTranslations += 1
    }

    for (bc <- backupComments)
      cs.print(Util.split(bc.value).map("#~ " + _).mkString("\n", "\n", "\n"))

    IO.writeUtf8ToFile(file, cs.close)

    if (missingTranslations == 1)
      println("  - Warning: 1 missing translation.")
    else if (missingTranslations >= 2)
      println("  - Warning: _ missing translations." << missingTranslations)
  }
}