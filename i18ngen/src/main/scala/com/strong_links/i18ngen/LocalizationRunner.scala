//package com.strong_links.i18ngen
//
//import com.strong_links.core._
//
//class LocalizationRunner(runConfig: RunConfig, i18nLocalization: I18nLocalization) extends LoggingPrefixed {
//
//  protected val loggingPrefixSeq: Seq[LoggingParameter] = Seq("Localization _" << i18nLocalization.i18nLanguageKey.string)
//
//  lazy val poFile = {
//    val inputDirectory = runConfig.inputRootDirectory
//    val file = i18nLocalization.fileFor(runConfig.inputRootDirectory, "po")
//    if (!file.exists) {
//      IO.writeUtf8ToFile(file, PoHeaderInfo.makeDefault(i18nLocalization))
//      logInfo("_ created with default contents." << file)
//    }
//    file
//  }
//
//  lazy val resourceFile = runConfig.getOutputFileFor(
//      i18nLocalization.fileFor(runConfig.outputDirectory, "scala")
//}