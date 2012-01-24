object Strange {

  def fmt(x: Double) = x.formatted("%.2f")

  def run(f: => Any) = {
    val n = 100000000L
    val start = System.nanoTime
    var i = 0L
    while (i < n) {
      f
      i += 1
    }
    val stop = System.nanoTime
    val delta = (stop - start).toDouble / 1000000000.0
    val perSecond = n.toDouble / delta
    val millionsPerSecond = perSecond / 1000000.0
    println(fmt(millionsPerSecond) + " millions per second (for " + fmt(delta) + " seconds).")
  }

  def main(args: Array[String]) = {
    args match {
      case Array("A") =>
        for (i <- 0 until 12) {
          run(1 + 1)
        }
      case Array("B") =>
        for (i <- 0 until 6) {
          run(1 + 1)
          run(1 + 1)
        }
      case Array("C") =>
        for (i <- 0 until 4) {
          run(1 + 1)
          run(1 + 1)
          run(1 + 1)
        }
      case Array("D") =>
        run(1 + 1)
        run(1 + 1)
        run(1 + 1)
        run(1 + 1)
        run(1 + 1)
        run(1 + 1)
        run(1 + 1)
        run(1 + 1)
        run(1 + 1)
        run(1 + 1)
        run(1 + 1)
        run(1 + 1)
      case Array("E") =>
        def go = run(1 + 1)
        go
        go
        go
        go
        go
        go
        go
        go
        go
        go
        go
        go
      case _ =>
        println("Bad input, use A, B or C.")
    }
  }
}