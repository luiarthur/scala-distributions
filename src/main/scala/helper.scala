package distribution

object helper {

  def timer[R](block: => R): R = {  
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1E9 + "s")
    result
  }

  def timerWithTime[R](block: => R): (R, Double) = {  
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    val elapsed = (t1 - t0) / 1E9
    println("Elapsed time: " + elapsed + "s")
    (result, elapsed)
  }

  def newton(init:Double, f:Double=>Double, fPrime:Double=>Double, eps:Double, maxIter:Int, verbose:Int): Double = {
    def engine(curr: Double, iter:Int): Double = {
      val next = curr - f(curr) / fPrime(curr)
      if (iter == maxIter) {
        if (verbose >= 1) println(s"Warning: newton method hasn't converged after ${maxIter} iterations!")
        next
      } else if (math.abs(curr - next) < eps) {
        if (verbose >= 2) println(s"newton method converged after ${iter} iterations!")
        next
      } else {
        engine(next, iter + 1)
      }
    }

    engine(init, 0)
  }
}
