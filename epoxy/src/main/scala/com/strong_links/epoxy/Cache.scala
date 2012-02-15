package com.strong_links.epoxy

import com.strong_links.core._

class Cache {

  private var sequence = 0

  private val out = new LeveledCharStream

  def store(callValue: String) = {
    val varName = "__cache" + sequence
    sequence += 1
    out.println("val _ = _" << (varName, callValue))
    varName
  }

  def get = out.close
}

