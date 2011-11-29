package net.strong_links.epoxy

import net.strong_links.core._

object ResourceStandAloneRunner extends StandAloneRunner {

  def programName = "ResourceStandAloneRunner"
  def inputDirectoryLabel = "Resource root directory"
  def outputDirectoryLabel = "Generated code root directory"

  def main(args: Array[String]): Unit = super.main(args)(logger => new ResourceScanner(logger))
}
