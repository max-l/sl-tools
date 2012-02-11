package com.strong_links.epoxy

import com.strong_links.core._

import java.io.File

class ResourceScanner extends EpoxyScanner {

  var stackTrace = false

  def loadResCompCacheFile(resCompCacheFile: File): List[FileInfo] = {
    if (resCompCacheFile.exists) {
      if (!resCompCacheFile.isFile)
        Errors.fatal("_ is not a file." << resCompCacheFile)
      val data = IO.loadUtf8TextFile(resCompCacheFile).replace("\r\n", "\n")
      for (line <- Util.split(data, '\n').zipWithIndex.filter(!_._1.isEmpty))
        yield FileInfo(resCompCacheFile, line._1, line._2 + 1)
    } else
      Nil
  }

  def processFiles(files: List[File], resCompCacheFile: File, directory: File, rootDirectory: File, outputDirectory: File, rootPackage: Option[String], rebuild: Boolean): File = {
    val segments = computePackageNameSegments(rootDirectory, directory, rootPackage)
    val fullPackageName = segments.mkString(".")
    val masterPackageSegments = segments.dropRight(1)
    val masterPackageName = masterPackageSegments.mkString(".")
    val packageName = segments.last
    val className = if (packageName(0).isUpper)
      packageName(0).toLower + packageName.substring(1)
    else
      packageName
    val objectName = className
    val outputDirectoryName = outputDirectory.path + IO.dirSeparator + segments.mkString(IO.dirSeparator)
    val outputFile = new File(outputDirectoryName + IO.dirSeparator + "package.scala")

    val oldMap = scala.collection.mutable.Map[String, FileInfo]()

    for (e <- loadResCompCacheFile(resCompCacheFile)) {
      if (oldMap.contains(e.name))
        Errors.fatal("File name _ exists twice in cache _." << (e.name, resCompCacheFile))
      oldMap(e.name) = e
    }

    val recycledMap = scala.collection.mutable.Map[String, FileInfo]()
    val newMap = scala.collection.mutable.Map[String, FileInfo]()

    for (f <- files) {
      if (oldMap.contains(f.getName)) {
        val old = oldMap(f.getName)
        oldMap.remove(f.getName)
        if (f.lastModified == old.lastModified)
          recycledMap(f.getName) = old
        else
          newMap(f.getName) = FileInfo(f)
      } else
        newMap(f.getName) = FileInfo(f)
    }

    val reuseCacheFile = resCompCacheFile.exists && oldMap.size == 0 && recycledMap.size == files.length && newMap.size == 0

    val recycledEntries = recycledMap.values.toList
    val newEntries = newMap.values.toList

    val entries = (recycledEntries ::: newEntries).sortWith(_.name < _.name)

    if (reuseCacheFile) {
      logDebug("Cache file has been reused.")
    } else {
      logDebug("Cache file has been recreated; _ MD5 entries recycled." <<< recycledEntries.length)
      IO.writeUtf8ToFile(resCompCacheFile, entries.map(e => e.name + "\t" + e.uuid + "\t" + e.lastModified).mkString("\n"))
    }

    val generate =
      if (outputFile.exists && !rebuild)
        outputFile.lastModified < resCompCacheFile.lastModified
      else
        true

    if (generate) {
      logDebug("Generating new _." <<< outputFile)
      val imports = List("com.strong_links.scalaforms.StaticUri")
      generateScalaFile(entries, "", outputFile, directory, masterPackageName, packageName, className, objectName, false, imports) { e =>
        val fileName = (packageName + IO.dirSeparator + e.nameUuid).replace(IO.dirSeparator, "/")
        val a = e.makeFunctionName(directory)
        val b = Convert.toScala(masterPackageName, true)
        val c = Convert.toScala(fileName, true)
        "def _ = new StaticUri(_, _)" << (a, b, c)
      }
    } else
      logDebug("_ is up-to-date." <<< outputFile)
    outputFile
  }

  def process(directory: File, rootDirectory: File, outputDirectory: File, rootPackage: Option[String], rebuild: Boolean): Option[File] = {

    val fname = "rescomp.cache"
    val resCompCacheFile = IO.makeFile(directory, fname)
    val resCompCacheFileName = resCompCacheFile.path

    // Check if there are files in this directory.
    val allFiles = directory.listFiles.toList.filter(_.isFile)
    val files = allFiles.filter(_.getName != fname)

    logDebug("Processing directory _" <<< directory)

    if (files.isEmpty) {
      logDebug("Directory empty, deleting _ if it exists." <<< resCompCacheFile)
      IO.deleteFile(resCompCacheFile, true)
      None
    } else {
      logDebug("Directory not empty, processing files.")
      Some(processFiles(files, resCompCacheFile, directory, rootDirectory, outputDirectory, rootPackage, rebuild))
    }
  }

  def scanFunction(file: File)(code: File => Unit) = {
    IO.processDirectories(file)(code(_))
  }
}

