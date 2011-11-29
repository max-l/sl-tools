package net.strong_links.epoxy

import net.strong_links.core._

import java.io.File

class ResourceScanner(logger: Logger) extends EpoxyScanner(logger) {

  var stackTrace = false

  def loadResCompCacheFile(resCompCacheFile: File): List[FileInfo] = {
    if (resCompCacheFile.exists) {
      if (!resCompCacheFile.isFile)
        Errors.fatal("File _ is not a file." << resCompCacheFile.getCanonicalPath)
      val data = IO.loadUtf8TextFile(resCompCacheFile).replace("\r\n", "\n")
      for (line <- data.split('\n').toList.zipWithIndex.filter(!_._1.isEmpty))
        yield FileInfo(resCompCacheFile, line._1, line._2 + 1)
    } else
      Nil
  }

  def processFiles(files: List[File], resCompCacheFile: File, directory: File, inputDirectory: File, outputDirectory: File, rootPackage: Option[String], rebuild: Boolean): Option[File] = {
    val segments = computePackageNameSegments(directory, inputDirectory, rootPackage)
    val fullPackageName = segments.mkString(".")
    val masterPackageSegments = segments.dropRight(1)
    val masterPackageName = masterPackageSegments.mkString(".")
    val packageName = segments.last
    val objectName = packageName
    val className = packageName.capitalize
    val outputDirectoryName = outputDirectory.getCanonicalPath + IO.dirSeparator + segments.mkString(IO.dirSeparator)
    val outputFile = new File(outputDirectoryName + IO.dirSeparator + "package.scala")
    val directoryForPackage = segments.mkString(IO.dirSeparator)

    val oldMap = scala.collection.mutable.Map[String, FileInfo]()

    for (e <- loadResCompCacheFile(resCompCacheFile)) {
      if (oldMap.contains(e.name))
        Errors.fatal("File name _ exists twice in cache file _." << (e.name, resCompCacheFile.getCanonicalPath))
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
      logger.debug("Cache file has been reused.")
    } else {
      logger.debug("Cache file has been recreated; _ MD5 entries recycled." <<< recycledEntries.length)
      IO.writeUtf8ToFile(resCompCacheFile, entries.map(e => e.name + "\t" + e.uuid + "\t" + e.lastModified).mkString("\n"))
    }

    val generate =
      if (outputFile.exists && !rebuild)
        outputFile.lastModified < resCompCacheFile.lastModified
      else
        true

    if (generate) {
      logger.debug("Generating new file _." <<< outputFile.getCanonicalPath)
      generateScalaFile(entries, outputFile, directory, masterPackageName, packageName, className, objectName, Nil) { e =>
        val cs = new LeveledCharStream
        cs.println("def _ = {" << e.makeFunctionName(directory))
        cs.increaseLevel
        val fileName = (directoryForPackage + IO.dirSeparator + e.nameUuid).replace(IO.dirSeparator, "/")
        cs.println(Convert.toScala(fileName, true))
        cs.decreaseLevel
        cs.println("}")
        cs.close
      }
      Some(outputFile)
    } else {
      logger.debug("File _ is up-to-date." <<< outputFile.getCanonicalPath)
      None
    }
  }

  def process(directory: File, inputDirectory: File, outputDirectory: File, rootPackage: Option[String], rebuild: Boolean): Option[File] = {

    val resCompCacheFileName = "rescomp.cache"
    val resCompCacheFullFileName = directory.getCanonicalPath + IO.dirSeparator + resCompCacheFileName
    val resCompCacheFile = new File(resCompCacheFullFileName)

    // Check if there are files in this directory.
    val files = directory.listFiles.toList.filter(_.isFile).filter(_.getName != resCompCacheFileName)

    logger.debug("Processing directory: _" <<< directory.getCanonicalPath)
    logger.debug("Number of files: _" <<< files.length)

    if (files.isEmpty) {
      logger.debug("Directory empty, deleting _ if it exists." <<< resCompCacheFile.getCanonicalPath)
      IO.deleteFile(resCompCacheFile, true)
      None
    } else {
      logger.debug("Directory not empty, processing files.")
      processFiles(files, resCompCacheFile, directory, inputDirectory, outputDirectory, rootPackage, rebuild)
    }
  }

  def scanFunction(file: File)(code: File => Unit) = {
    IO.processDirectories(file)(code(_))
  }
}

