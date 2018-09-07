package distribution.continuous

import distribution.Univariate
import distribution.RandomGeneric
import math.{log, exp}

case class Uniform(params: (Double,Double)) extends Univariate(params) {

  type RvType = Double

  def this(a:Double, b:Double) {
    this( (a, b) )
  }

  val (a, b) = params
  //require(sd > 0, "In Normal(mean, sd): sd > 0 required!")

  def inSupport(x:Double):Boolean = a <= x && x <= b

  val mean:Double = (a + b) / 2
  val variance:Double = math.pow(b - a, 2) / 12.0
  val range:Double = b - a
  private val preComputedPdf = 1.0 / range
  private val preComputedLogPdf = -math.log(range)

  override def lpdf(x:Double):Double = if (inSupport(x)) {
    preComputedLogPdf
  } else {
    Double.NegativeInfinity
  }

  def pdf(x:Double):Double = preComputedPdf
  def cdf(x:Double):Double = x match {
    case y if inSupport(y) => (y - a) / range
    case y if y > b => 1.0
    case _ => 0.0
  }

  override def toString = {
    s"Beta(a:$a, b:$b)"
  }

  def sample[Rng <: distribution.RandomGeneric](rng: Rng):Double = {
    rng.nextUniform(a, b)
  }
}
