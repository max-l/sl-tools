package net.strong_links.i18ngen

import net.strong_links.core._

import java.io.File
import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
 
object StaticCode {
    
  val body =
"""
  // Map to transform a key string into an index internal to this class.
    
  private val h = new java.util.IdentityHashMap[String, Int](nbEntries)

  // Load the map
    
  private var i = 0
  while (i < keyTable.length) { 
    h.put(keyTable(i), i)    
    i += 1
  }

  // The keyTable is no longer needed, we will use 'h' instead as the main entry point.
    
  keyTable = null
    
  // Metadata
    
  val languageKey = "_"
  val nbEntries = _ 
  val nbPluralForms = _ 
  val pluralForms = "_" 
  val generatedAt= "_"
  val javaVersion = "_"

  // (End of constructor)

  // Access the translations

  def gettext(key: String): String = {
    if (h.containsKey(key))
      translationTable(firstTranslationTable(h.get(key)))
    else    
      ""
  }

  def ngettext(key: String, n: Int): String = {
    val pluralForm = computePluralForm(n)
    if ((pluralForm >= 0) && (pluralForm < nbPluralForms) && h.containsKey(key))
      translationTable(firstTranslationTable(h.get(key)) + pluralForm)
    else    
      ""
  }
"""

val flatten = 
"""
  private def flatten[T: ClassManifest](arrays: Array[Array[T]]): Array[T] = {
    var flattenArraySize = 0
    var i = 0
    while (i < arrays.length) {
      flattenArraySize += arrays(i).length
      i += 1
    }
    val flattenArray = new Array[T](flattenArraySize)
    var k = 0
    i = 0
    while (i < arrays.length) {
      Array.copy(arrays(i), 0, flattenArray, k, arrays(i).length)
      k += arrays(i).length
      i += 1
    }
    if (k != flattenArraySize)
      throw new Exception("Size mismatch while copying arrays, " + k + " vs. " + flattenArraySize)
    flattenArray
   }
"""  
}

class TableDef[T <: Any](name: String, data: Array[T], isVar: Boolean) {
  val MAX_ARRAY_ENTRIES = 500
  val tableType = if (data.isInstanceOf[Array[Int]]) {
    val max = data.asInstanceOf[Array[Int]].max
    if (max < 128) "Byte" else if (max < 32768) "Short" else "Int"
  } else {
    "String"
  }

  def generateArray(start: Int, length: Int) = {
    for (i <- start until start + length)
      yield data(i) match {
        case null => "null" 
        case s: String => "\"" + s + "\""
        case x => x.toString
      }
  }
  
  def methodName(suffix: String) = {
    "build" + name.capitalize + suffix
  }

  def generateDef: String = {
    Util.withStringBuilder { sb => build("", 0, data.length, sb) }
  }

  def build(suffix: String, start: Int, length: Int, sb: StringBuilder): String = {
    val body = 
      if (length > MAX_ARRAY_ENTRIES) {
        val subBuild = ListBuffer[String]()
        var i = start
        var left = length
        while (left != 0) {
          val thisTime = left min MAX_ARRAY_ENTRIES
          subBuild += build(i.toString, i, thisTime, sb)
          left -= thisTime
          i += thisTime
        }
        val arrayStart = "    flatten(Array[Array[_]](" << tableType
        subBuild.mkString(arrayStart, ", ", "))")
      } else {
        val arrayStart = "    Array[_](" << tableType
        generateArray(start, length).mkString(arrayStart, ", ", ")")
      }
    sb.append("\n")
    sb.append("  private def _: Array[_] = {\n" << (methodName(suffix), tableType))  
    sb.append(body); sb.append("\n")
    sb.append("  }")  
    sb.append("\n")
    methodName(suffix)
  }

  def generateVal: String = {
    "  private _ _ = _" << (if (isVar) "var" else "val", name, methodName(""))
  }
}

class ResourceFileWriter(file: File, className: String, languageKey: String, nbPluralForms: Int, 
                         pluralForms: String, entries: List[PoEntry], loggers: Logger) {

  val pw = new CharStream
  
  def generateStaticCode(timestamp: String, nbEntries: Int, nbPluralForms: Int, 
                         pluralForms: String): String = {
    
    StaticCode.body << (languageKey, nbEntries, nbPluralForms, pluralForms, timestamp, System.getProperty("java.version"));
  }

  def generateAndClose {

    val nbEntries = entries.length
    
    // Allocate tables
    val keyTable = new Array[String](nbEntries)
    val firstTranslationTable = new Array[Int](nbEntries)
    val translationTableBuffer = new ArrayBuffer[String]()

    // Compute the table entries.
    for (k <- 0 until nbPluralForms)
      translationTableBuffer += "" 
    for ((e, i) <- entries.zipWithIndex) {
      keyTable(i) = e.msgCtxt match {
        case None => e.msgid
        case Some(ctx) => ctx + "\\u0000" + e.msgid
      }
      if (e.hasSomeTranslations) {
        firstTranslationTable(i) = translationTableBuffer.length
        for (t <- e.translations)
          translationTableBuffer += t
      } else {
        firstTranslationTable(i) = 0
      }
    }
    val translationTable = translationTableBuffer.toArray

    val tables = List(
      new TableDef("keyTable", keyTable, true),
      new TableDef("firstTranslationTable", firstTranslationTable, false),
      new TableDef("translationTable", translationTable, false))
        
    val now = Util.nowAsString
    pw.println("// -------------_" << ("-" * Util.nowAsString.length))
    pw.println("// Generated on _" << now)
    pw.println("// -------------_" << ("-" * Util.nowAsString.length))

    pw.println
    pw.println("class _ {" << className)

    pw.println
    pw.println("  // Create the required tables.")
    pw.println
    for (t <- tables)
      pw.println(t.generateVal)

    pw.println(generateStaticCode(now, nbEntries, nbPluralForms, pluralForms))
    
    val compiler = new PluralFormsCompiler(pluralForms, loggers)
    val pluralFormsMethod = compiler.compile

    pw.println("  // Compute the required plural form for a given 'n'.")
    pw.println
    pw.println(pluralFormsMethod)
    pw.println

    pw.println("  // Generic array flattening method.") 
    pw.println(StaticCode.flatten)

    //val typesDone = collection.mutable.Set[String]()
    //for (t <- tables if (!typesDone.contains(t.tableType))) {
      //typesDone += t.tableType
      //pw.println(generateFlattenMethodForType(t.tableType))
    //}

    pw.println("  // Build the tables.")
    for (t <- tables)
      pw.println(t.generateDef)

    pw.println("}")

    IO.writeUtf8ToFile(file, pw.close)
  }
}