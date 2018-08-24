package distributions

object Random {
  // See common distributions to implement
  //http://commons.apache.org/proper/commons-math/javadocs/api-3.6/org/apache/commons/math3/random/RandomDataGenerator.html


  // For Production
  //val R = java.util.concurrent.ThreadLocalRandom.current()

  // For Testing...
  val R = scala.util.Random
  R.setSeed(10)

  def round(x:Double, d:Int) = {
    require(d >= 0)
    val dMax = x.toString.split('.').last.size
    val factor = math.pow(10, if(d>dMax) dMax else d)
    (x * factor).toInt / factor
  }

  private def rU() = R.nextDouble

  // univairate continuous
  def runif(a: Double=0, b: Double=1) = {
    require(b > a)
    rU * (b-a) + a
  }

  /* Reference implementatoin using optional variance
  def rnorm(mean: Double=0, sd: Double=1, variance: Option[Double]=None) = {
    require(sd > 0)
    variance match {
      case Some(v) => R.nextGaussian * math.sqrt(v) + mean
      case _ => R.nextGaussian * sd + mean
    }
  }
  */

  def rnorm(mean: Double=0, sd: Double=1) = {
    require(sd > 0)
    R.nextGaussian * sd + mean
  }


  def rexp(rate: Double=1) = {
    require(rate > 0)
    -math.log(1 - rU) / rate
  }
  
  private def rgammaRateIsOneShapeGeOne(shape:Double): Double = {
    val d = shape - 1.0 / 3.0
    val c = 1.0 / math.sqrt(9*d)

    def engine(): Double = {
      val z = R.nextGaussian
      lazy val u = rU()
      lazy val v = math.pow(1 + c*z, 3)
      if (z > -1.0/c && math.log(u) < z*z/2.0 + d*(1-v+math.log(v))) {
        d * v
      } else engine()
    }

    engine()
  }

  def rgamma(shape:Double, rate:Double):Double = {
    require(shape > 0 && rate > 0)
    if (shape >= 1) {
      rgammaRateIsOneShapeGeOne(shape) / rate
    } else {
      rgamma(shape+1, rate) * math.pow(rU,1/shape)
    }
  }


  /** random draw from inverse gamma where 
   *  - expected value is b/(a-1)
   *  - variance is b^2/( (a-1)^2 * (a-2)), for a > 2
   */
  def rinvgamma(a:Double, b:Double):Double = {
    1 / rgamma(a, b)
  }

  def rbeta(a:Double, b:Double):Double = {
    require(a > 0 && b > 0)
    lazy val x = rgamma(a, 1)
    lazy val y = rgamma(b, 1)
    x / (x + y)
  }

  def rF = ???

  def rtdist = ???

  def rweib = ???

  def rchisq = ???

  // Discrete univariate
  def rgeom = ???

  def rnegbinom = ???

  def rpois = {
    //https://www.johndcook.com/blog/2010/06/14/generating-poisson-random-values/???
    ???
  }

  def rbern(p:Double):Int = {
    require(p >= 0 && p <= 1)
    if (p > R.nextDouble) 1 else 0
  }

  def rbinom(n:Int, p:Double):Int = {
    require(n >= 0 && p >= 0 && p <= 1)
    List.fill(n)(rbern(p)).sum
  }


}
