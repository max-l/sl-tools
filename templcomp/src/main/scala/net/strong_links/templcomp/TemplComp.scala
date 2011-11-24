package net.strong_links.templcomp

import net.strong_links.core._

import java.io.File
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map => MapBuffer}
import scala.collection.mutable.ArrayBuffer

class TemplateParser(file: File, cs: CharStream) extends BasicLexParser(IO.loadUtf8TextFile(file)) {

  def nameOf(c: Class[_]) = {
    val segments = c.getName.split('.')
    if (segments.isEmpty)
      Errors.fatal("No dot found in _." << c.getName)
    segments.last
  }
  
  val T_BASE_FIELD = "BaseField[_]"
  val T_GENERAL_STRING = nameOf(classOf[GeneralString])
  val T_XML = nameOf(<a />.getClass)
  
  class Member(val name: String, val firstUseLine: Int, val baseType: String)

  class Argument(val name: String) {
    
    var firstUseLine = -1
    var isObject = false
    val members = MapBuffer[String, Member]()
    lazy val finalMembers = members.values.toList.sortWith(_.name < _.name)
    
    def makeType = {
      if (isObject)
         templateName.capitalize + "_" + name.capitalize
       else
         members("").baseType
    }
  }
  
  import LexSymbol._

  override def getFileName = Some(file.getCanonicalPath)

  override def getSymbolHere {
    if (currentChar == '<' && nextChar == '!' && nextNextChar == '-' && nextNextNextChar == '-') {
      move(4)
      symbol = htmlStartComment
    } else if (currentChar == '-' && nextChar == '-' && nextNextChar == '>') {
      move(3)
      symbol = htmlEndComment
    } else
      super.getSymbolHere
  }

  var templates = MapBuffer[String, Int]()
  var inTemplate = false
  var templateName = ""
  var arguments = List[Argument]()
  var verbatimPos = -1

  var body = new CharStream
  
  def makeArgList = {
    if (arguments.isEmpty)
      ""
    else
      arguments.map(arg => arg.name + ": " + arg.makeType).mkString("(", ", ", ")")
  }

  def templateStart {
    inTemplate = true
    getSymbol
    expect(identifier)
    val declarationLineNumber = startLineNumber
    templateName = symbolValue
    if (!templateName(0).isLower)
      error(startLineNumber, "The template name _ does not start with a lowercase letter." << templateName)
    if (templates.contains(templateName))
      error(startLineNumber, "The template _ has already been declared near line _." << 
            (templateName, templates(templateName)))
    templates += (templateName -> declarationLineNumber)
    getSymbol
    val args = scala.collection.mutable.ListBuffer[String]()
    expect(Set(leftParenthesis, htmlEndComment))
    def getArgument {
      getSymbol
      expect(identifier)
      if (symbolValue(0) != '$')
        error(startLineNumber, "The argument name _ does not start with a $ sign." << symbolValue)
      args += symbolValue.substring(1)
      getSymbol
    }
    if (symbol == leftParenthesis) {
      getArgument
      while (symbol == comma)
        getArgument
      skip(rightParenthesis)
    }
    skip(htmlEndComment)
    verbatimPos = startPos
    arguments = args.toList.map(name => new Argument(name))
    body = new CharStream
  }

  def templateEnd(endPos: Int) {
    if (inTemplate) {
      verbatim(endPos, true)
      val unusedArguments = arguments.filter(_.finalMembers.isEmpty).map(_.name)
      if (!unusedArguments.isEmpty)
        error(startLineNumber, "Unused arguments: _" << unusedArguments)
      val isUsingField = arguments.exists(_.finalMembers.exists(_.baseType == T_BASE_FIELD))
      cs.println
      cs.println("  def _1_2(os: OutStream) {" << (templateName, makeArgList))
      if (isUsingField)
        cs.println("    val ft = fieldTransformer.get")
      cs.print(body.close)
      cs.println("  }")
      for (arg <- arguments; if arg.isObject) {
        cs.println
        cs.println("  type _ = {" << arg.makeType)
        if (arg.finalMembers.isEmpty)
          warning(startLineNumber, "Argument $_ is never referenced in the template _." << 
                  (arg.name, templateName))
        for (member <- arg.finalMembers)
          cs.println("    def _: _" << (member.name, member.baseType))
        cs.println("  }")
      }
      inTemplate = false
    }
  }

  def massage(s: String, trimAllSpaces: Boolean) = {
    val addSpaceLeft = !trimAllSpaces && (s.length > 0) && s(0).isSpaceChar 
    val addSpaceRight = !trimAllSpaces && (s.length > 0) && s(s.length - 1).isSpaceChar 
    val w = s.trim
    var r = (addSpaceLeft, addSpaceRight) match {
      case (false, false) => w 
      case (false, true) => w + " "
      case (true, false) => " " + w
      case (true, true) => " " + w + " "
    }
    val commentStart = "<!--"
    val commentEnd = "-->"
    var done = false
    while (!done) {
      val pos = r.indexOf(commentStart)
      done = pos == -1
      if (!done) {
        val z = r.indexOf(commentEnd, pos + commentStart.length)
        if (z == -1)
          error(startLineNumber, "Unclosed HTML comment.") 
        r = r.substring(0, pos) + r.substring(z + commentEnd.length)
      }
    }
    r
  }
  
  def verbatim(endPos: Int, templateEnding: Boolean) {
    if (inTemplate && verbatimPos != -1) {
      val s = Convert.toScala(massage(data.substring(verbatimPos, endPos), templateEnding))
      if (!s.isEmpty)
        body.println("    os.write(\"" + s + "\")")
    }
  }

  def isArgumentName(arg: String) = {
    arg.startsWith("$") && arg.length > 1 && isIdentifierCharacter(arg(1))
  }

  def processHtmlComment(pre: => Unit) {
    val savedPos = startPos
    getSymbol
    if (symbol == template || symbol == LexSymbol.templateEnd) {
      val start = symbol == template
      templateEnd(savedPos)
      if (templates.keys.toList.length == 0)
        pre
      if (start)
        templateStart
    }
  }

  def processReference(arg: Argument, lineNumber: Int, initialPos: Int) {

    verbatimPos = initialPos

    // Check if first use, and if so, whether the usage as object/non object is consistent.
    def ono(isObject: Boolean) = if (isObject) "an object" else "a non object"
    val isObject = (symbol == dot)
    val firstUse = arg.firstUseLine == -1
    if (firstUse)
      arg.firstUseLine = lineNumber
    else if (arg.isObject != isObject)
      error(startLineNumber, "Argument _ is used as _ but it was previously used as _ on line _ (incompatible base types)." << 
            (arg.name, ono(isObject), ono(arg.isObject), arg.firstUseLine))
    arg.isObject = isObject

    // Get the member name. We'll use "" as a dummy member name when the object notation is not used.
    val (memberName, fullMemberName) = 
      if (symbol == dot) {
        getSymbol
        expect(identifier)
        val x = symbolValue
        if (!x(0).isLower)
          error(startLineNumber, "Member name _ does not start with a lowercase letter." << x)
        // Position now just after the member name: : $arg.member^
        verbatimPos = pos
        getSymbol
        (x, arg.name + "." + x) 
      } else
        ("", arg.name)

    val usage = 
      if (symbol == colon) {
        getSymbol
        expect(identifier)
        val x = symbolValue
        // Position now just after the usage: : $arg:js^ or $arg.member:js^
        verbatimPos = pos
        getSymbol
        x
      } else
        ""
    
    val (baseType, renderingCode) = usage match {
      case "" => (T_GENERAL_STRING, "os.write(toHtml(_.toString))" << fullMemberName)
      case "js" => (T_GENERAL_STRING, "os.write(toJs(_.toString, true))" << fullMemberName)
      case "raw" => (T_GENERAL_STRING, "os.write(_.toString)" << fullMemberName)
      case "xml" => (T_XML, "os.write(_.toString)" << fullMemberName)
      case "field" => (T_BASE_FIELD, "ft.transform(_).render(os)" << fullMemberName)
      case "fieldLabel" => (T_BASE_FIELD, "ft.transform(_).renderLabel(os)" << fullMemberName)
      case "fieldControl" => (T_BASE_FIELD, "ft.transform(_).renderControl(os)" << fullMemberName)
      case "fieldHelp" => (T_BASE_FIELD, "ft.transform(_).renderHelp(os)" << fullMemberName)
      case "fieldError" => (T_BASE_FIELD, "ft.transform(_).renderError(os)" << fullMemberName)
      case _ => Errors.fatal("Invalid usage type _ for _." << (usage, fullMemberName))
    }

    if (arg.members.contains(memberName)) {
      val m = arg.members(memberName)
      if (m.baseType != baseType)
        error(startLineNumber, "Argument _ is used as _ but it was previously used as _ on line _ (incompatible base types)." << 
              (fullMemberName, baseType, m.baseType, m.firstUseLine))
    } else
      arg.members += (memberName -> new Member(memberName, lineNumber, baseType))
    body.println("    _" << renderingCode)
  }
  
  def processDollar(dollarPos: Int, lineNumber: Int) {
    val argumentName = symbolValue.substring(1)
    val posAfterArgumentName = pos
    getSymbol
    arguments.find(_.name == argumentName) match {
      case None =>
      case Some(argument) => 
        verbatim(dollarPos, false)
        processReference(argument, lineNumber, posAfterArgumentName)
    }
  }

  def compile(pre: => Unit)(post: => Unit){
    getSymbol
    while (symbol != eof) symbol match {
      case `htmlStartComment` =>
        processHtmlComment(pre)
      case `identifier` if (inTemplate && isArgumentName(symbolValue)) =>
        processDollar(startPos, startLineNumber)
      case _ =>
        getSymbol
    }
    templateEnd(pos)
    if (templates.keys.toList.length != 0)
      post
  }
}

class TemplComp {

  // Constants.
  val PROGRAM_NAME = "templComp"

  var stackTrace = false
  
  def addEndDot(s: String) = {
    if (!s.isEmpty && !s.endsWith("."))
      s + "."
    else
      s
  } 

  def extractPackageName(root: String, file: File) = {
    val directoryName = file.getParentFile.getCanonicalPath
    val partialPath = directoryName.substring(root.length)
    partialPath.split(IO.dirSeparator(0)).filter(!_.isEmpty).mkString(".")
  }

  def extractName(file: File) = {
    val segments = file.getName.split('.').filter(!_.isEmpty)
    if (segments.length < 2)
      Errors.fatal("Invalid template file name _." << file.getCanonicalPath)
    val segmentsWithoutExtension = segments.dropRight(1)
    segmentsWithoutExtension.mkString
  }

  def getArguments(args: Array[String]) = {
    var htmlFilesRoot, outputFilesRoot, packageRoot = ""
    var rebuild = false
    import CommandLine._
    val config = List(
      Parameter("HTML files root directory", "Path where the HTML files reside.", htmlFilesRoot = _),
      Parameter("Output files root directory", "Path where the Scala files will be generated.", outputFilesRoot = _),
      SimpleSwitch("rebuild", "Always generate the Scala files.", rebuild = true),
      SimpleSwitch("stackTrace", "Show stack trace upon error.", stackTrace = true),
      ValuedSwitch("packageRoot", "package root", "Package root.", packageRoot = _))
    new CommandLine(PROGRAM_NAME, config).run(args)

    val htmlFilesRootFile = IO.toDirectoryFile(htmlFilesRoot)
    val outputFilesRootFile = IO.toDirectoryFile(outputFilesRoot, true)

    (htmlFilesRootFile, outputFilesRootFile, packageRoot, rebuild)
  }

  // Note: file names written without backslashes because it makes Eclipse complain
  //       about invalid unicode sequences...
  def header(cs: CharStream, sourceFileName: String, destinationFileName: String) {
    def r(s: String) = s.replace("\\", "/")
    val of = r(destinationFileName)
    val sf = r(sourceFileName)
    val now = Util.nowAsString
    val pad = 18
    val stars = (80 /: Seq(of, sf, now))(_ max _.length + pad) - 1
    cs.println("/" + "*" * stars)    
    cs.println("*")
    cs.println("* Output file:    _" << of)
    cs.println("*")
    cs.println("* Source file:    _" << sf)
    cs.println("*")
    cs.println("* Generated on:   _" << now)
    cs.println("*")
    cs.println("*" * stars + "/")    
    cs.println
  }
  
  def processFile(htmlFilesRootFilePath: String, packageRoot: String, outputFilesRootFile: File,
    file: File, rebuild: Boolean): File = {
    val T_FULL_XML = <a />.getClass.getCanonicalName
    val filePackage = extractPackageName(htmlFilesRootFilePath, file)
    val packageName = List(packageRoot, filePackage).filter(!_.isEmpty).mkString(".")
    if (packageName.isEmpty)
      Errors.fatal("Can't generate a Scala file for _ as there is no package." << file.getCanonicalPath)
    val name = extractName(file)
    val className = name.capitalize
    val directoryForPackage = filePackage match {
      case "" => IO.dirSeparator
      case x => x.split('.').mkString(IO.dirSeparator, IO.dirSeparator, IO.dirSeparator)
    }
    val outputDirectoryName = outputFilesRootFile.getCanonicalPath + directoryForPackage
    IO.createDirectory(new File(outputDirectoryName), true)
    val outputFile = new File(outputDirectoryName + className + ".scala")

    val generate =
      if (outputFile.exists && !rebuild)
        file.lastModified > outputFile.lastModified
      else
        true

    if (generate) {
      println("  - Processing _." <<< file.getCanonicalPath)
      println("    > Output _." <<< outputFile.getCanonicalPath)
      println("    > Package _." <<< packageName)
      val contents = IO.usingCharStream { cs =>
        val parser = new TemplateParser(file, cs)
        parser.compile {
          header(cs, file.getCanonicalPath, outputFile.getCanonicalPath)
          cs.println("package _" << packageName)
          cs.println
          cs.println("import net.strong_links.core._")
          cs.println("import net.strong_links.core.Convert._")
          cs.println("import net.strong_links.scalaforms.BaseField")
          cs.println("import net.strong_links.scalaforms.OutStream")
          cs.println("import net.strong_links.scalaforms.fieldTransformer")
          cs.println
          cs.println("import _" << T_FULL_XML)
          cs.println
          cs.println("class _ {" << className)
        } {
          cs.println("}")
          cs.println
          cs.println("object _1 extends _1" << className)
        }
      }
      if (contents.isEmpty)
        IO.deleteFile(outputFile, true)
      else
        IO.writeUtf8ToFile(outputFile, contents)
    }
    
    outputFile
  }

  def process(sourceRoot: File, suppliedPackageRoot: String, outputDir: File): Seq[File] = {

    val args = (List(sourceRoot.getCanonicalPath, outputDir.getCanonicalPath, "--rebuild", "--stackTrace") ::: 
                 (if (suppliedPackageRoot == "") Nil else List("--packageRoot=" + suppliedPackageRoot))).toArray
      
    val (htmlFilesRootFile, outputFilesRootFile, packageRoot, rebuild) = getArguments(args)

    val files = new ArrayBuffer[File]
    
    IO.scanDirectory(sourceRoot)(f => {
      val r = processFile(htmlFilesRootFile.getCanonicalPath, packageRoot, outputFilesRootFile, f, true)      
      files.append(r)
    })
    
    files
  }
  
  
  def run(args: Array[String]) {
    try {
      // Get the command line arguments.
      val (htmlFilesRootFile, outputFilesRootFile, packageRoot, rebuild) = getArguments(args)
  
      // Name of the HTML root full path.
      val htmlFilesRootFilePath = htmlFilesRootFile.getCanonicalPath
  
      // Emit informational messages.
      println("Running _ on root directory _." <<< (PROGRAM_NAME, htmlFilesRootFilePath))
      println("  - Output files root directory: _." <<< outputFilesRootFile.getCanonicalPath)
      println("  - Package root: _." <<< (packageRoot match { case "" => "(none)"; case x => x }))
  
      // Process each file found in the tree or subtree.
      IO.scanDirectory(htmlFilesRootFile)(
        processFile(htmlFilesRootFilePath, packageRoot, outputFilesRootFile, _, rebuild))
      
    } catch {
      case e: Exception => 
        if (stackTrace)
          throw e
        else
          Console.err.println("Error: _" << e.getMessage)
    }
  }
}


import sbt.Keys._
import sbt._

object TemplComp extends sbt.Plugin {

 def main(args: Array[String]) {
   val tc = new TemplComp
   tc.run(args)
 }
 

 def apply(templateDir: String, parentPackage: String = "") = 
   (sourceGenerators in Compile) <+= (sourceManaged in Compile) map { dir =>
      val tc = new TemplComp
      tc.process(file(templateDir), parentPackage, dir)      
    }   
 
 override lazy val settings = Seq(commands += generateTemplates)

  lazy val generateTemplates = 
    Command.command("gen-templates") { (state: State) =>
	  val projDir = new File(".").getCanonicalPath
	  main(Array("./faw/src/main/templates", "./faw/src/main/generatedCode", "--rebuild", "--stackTrace"))
      state
    }  
}
