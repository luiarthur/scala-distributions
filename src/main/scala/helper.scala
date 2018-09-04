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


}
