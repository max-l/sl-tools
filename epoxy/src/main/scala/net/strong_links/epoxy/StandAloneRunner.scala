package net.strong_links.epoxy

import net.strong_links.core._

import java.io.File

import CommandLine._

abstract class StandAloneRunner {

  import CommandLine._

  def programName: String
  def inputDirectoryLabel: String
  def outputDirectoryLabel: String

  var stackTrace = false

  def execute(logger: Xlogger, args: Array[String])(scannerCreator: ScannerCreator) = {

    var inputDirectory: Option[File] = None
    var outputDirectory: Option[File] = None
    var rootPackage: Option[String] = None
    var rebuild = false

    val config = List(
      Parameter("input directory", inputDirectoryLabel, id => inputDirectory = Some(new File(id))),
      Parameter("output directory", outputDirectoryLabel, od => outputDirectory = Some(new File(od))),
      SimpleSwitch("rebuild", "Always rebuild all output files.", rebuild = true),
      SimpleSwitch("stackTrace", "Show stack trace upon error.", stackTrace = true),
      SimpleSwitch("debug", "Show debug log entries.", logger.enableDebug = true),
      ValuedSwitch("rootPackage", "rootPackage", "Root package.", pr => rootPackage = Some(pr)))

    CommandLine(programName, config, args)

    (inputDirectory, outputDirectory) match {
      case (Some(id), Some(od)) =>
        logger.info("_ started." << programName)
        scannerCreator(logger).run(id, od, rootPackage, rebuild)
        logger.info("_ ended." << programName)
      case _ =>
        Errors.fatal("Logic error; parameters can't be None at this point.")
    }
  }

  def main(args: Array[String])(scannerCreator: ScannerCreator) = {

    object logger extends StandAloneLogger

    try
      execute(new Xlogger(logger), args)(scannerCreator)
    catch {
      case e: Exception =>
        if (stackTrace)
          throw e
        else
          logger.error(e.getMessage)
    }
  }
}

