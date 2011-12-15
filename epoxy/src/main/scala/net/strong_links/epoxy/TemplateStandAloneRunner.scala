package net.strong_links.epoxy

import net.strong_links.core._

object TemplateStandAloneRunner extends StandAloneRunner {

  def programName = "TemplateStandAloneRunner"
  def inputDirectoryLabel = "Template root directory."
  def outputDirectoryLabel = "Generated code root directory."

  def main(args: Array[String]): Unit = super.main(args)(new TemplateScanner)
}