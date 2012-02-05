package com.strong_links.i18ngen

import com.strong_links.core._
import java.io.File
import java.util.Locale

object I18nShowConfig extends Logging {

  import Console._

  def run(runConfig: RunConfig): Seq[File] = {

    runConfig.i18nConfigs.sortWith(_.packageName < _.packageName).foreach(_.showConfig)
    println

    Nil
  }
}

