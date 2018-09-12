package distribution.continuous

import distribution.UnivariateContinuous
import distribution.RandomGeneric
import org.apache.commons.math3.special.Gamma._
import org.apache.commons.math3.special.Beta._
import math.{log, exp}

case class Beta(params: (Double,Double)) extends UnivariateContinuous(params) {
  type RvType = Double

  def this(a:Double, b:Double) {
    this( (a, b) )
  }

  def inSupport(x:RvType) = 0 < x && x < 1

  val (a, b) = params
  require(a > 0 && b > 0, "In Beta(a, b): a, b > 0 required!")

  lazy val mean = a / (a + b)
  lazy val variance = mean * mean * b / (a * (a + b + 1))
  lazy val min = 0
  lazy val max = 1
  lazy val mode = (a, b) match {
    case (1, 1) => 0.5 // in fact, any value in support
    case (aa, bb) if aa > 1 && bb > 1 => (a - 1) / (a + b - 2)
    case (1, bb) if bb > 1 => 0
    case (aa, 1) if aa > 1 => 1
    case _ => Double.NaN // not defined
  }

  override def lpdf(x:RvType):Double = if (inSupport(x)) {
    logGamma(a + b) - logGamma(a) - logGamma(b) + (a - 1) * log(x) + (b - 1) * log(1 - x) 
  } else {
    Double.NegativeInfinity
  }

  def pdf(x:RvType):Double = math.exp(lpdf(x))
  def cdf(x:RvType):Double = x match {
    case y if inSupport(y) => regularizedBeta(y, a, b)
    case y if y >= 1.0 => 1.0
    case _ => 0.0
  }

  override def toString = {
    s"Beta(a:$a, b:$b)"
  }

  def sample[Rng <: distribution.RandomGeneric](rng: Rng):RvType = {
    rng.nextBeta(a, b)
  }

  // TODO: override quantile for case when mode is not defined
}
