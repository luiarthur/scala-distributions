package distributions


trait RandomGeneric2 {
  // Methods to Implement
  def setSeed(seed:Long): Unit
  def nextBoolean(): Boolean
  def nextInt(n:Int): Int
  def nextLong(): Long
  def nextFloat(): Float
  def nextDouble(): Double

  // Methods to inherit
  def nextGaussian(): Double = {
    // box-mueller
    math.sqrt(-2 * math.log(nextDouble)) * math.cos(2 * math.Pi * nextDouble)
  }

  def nextUniform(a:Double=0, b:Double=1): Double = if (a == 0 &&  b == 0) {
    nextDouble()
  } else {
    a + nextDouble() * (b - a)
  }

  // Univariate Continuous
  def nextExponential(lam:Double): Double = ???
  def nextGamma(shape:Double, rate:Double): Double = ???
  def nextInvGamma(shape:Double, rate:Double): Double = ???
  def nextBeta(a:Double, b:Double): Double = ???
  def nextChisq(nu:Double): Double = ???
  def nextF(df1:Double, df2:Double): Double = ???
  def nextT(df:Double): Double = ???
  def nextWeibull(shape:Double, scale:Double): Double = ???
  def nextLogistic(mean:Double, scale:Double): Double = ???


  // Univariate Discrete

  // Multivariate Continuous

  // Multivariate Discrete
}

class RandomSeq(rng: scala.util.Random) extends RandomGeneric2 {
  def setSeed(seed:Long):Unit = rng.setSeed(seed)
  def nextBoolean(): Boolean = rng.nextBoolean()
  def nextInt(n:Int):Int = rng.nextInt(n)
  def nextLong(): Long = rng.nextLong()
  def nextFloat(): Float = rng.nextFloat()
  def nextDouble():Double = rng.nextDouble
}

object RandomPar extends RandomGeneric2 {
  import java.util.concurrent.{ ThreadLocalRandom => rng }
  def setSeed(seed:Long):Unit = rng.current().setSeed(seed)
  def nextBoolean(): Boolean = rng.current().nextBoolean()
  def nextInt(n:Int):Int = rng.current().nextInt(n)
  def nextLong(): Long = rng.current().nextLong()
  def nextFloat(): Float = rng.current().nextFloat()
  def nextDouble():Double = rng.current().nextDouble
}


