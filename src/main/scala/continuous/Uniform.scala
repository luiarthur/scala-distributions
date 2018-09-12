package distribution.continuous

import distribution.UnivariateContinuous
import distribution.RandomGeneric
import math.{log, exp}

case class Uniform(params: (Double,Double)) extends UnivariateContinuous(params) {

  type RvType = Double

  def this(a:Double, b:Double) {
    this( (a, b) )
  }

  lazy val (a, b) = params
  require(b > a, "Uniform(a, b): b > a required!")

  def inSupport(x:Double):Boolean = a <= x && x <= b

  lazy val mean:Double = (a + b) / 2
  lazy val variance:Double = math.pow(b - a, 2) / 12.0
  lazy val range:Double = b - a
  private lazy val preComputedPdf = 1.0 / range
  private lazy val preComputedLogPdf = -math.log(range)

  lazy val min = a
  lazy val max = b
  lazy val mode = (a + b) / 2


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
