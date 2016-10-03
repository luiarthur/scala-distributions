package distributions

object Random {

  import math.{log}

  val R = scala.util.Random

  private def rU = R.nextDouble

  def runif(a: Double=0, b: Double=1) = {
    assert(b > a)
    rU * (b-a) + a
  }

  def rnorm(mean: Double=0, sd: Double=1) = {
    assert(sd > 0)
    R.nextGaussian * sd + mean
  }

  def rexp(rate: Double=1) = {
    assert(rate > 0)
    - log(1-rU) / rate
  }
  
  def rgamma = ???

  def rigamma = ???

  def rbeta = ???

  def rF = ???

  def rtdist = ???

  def rweib = ???

  def rchisq = ???
}
