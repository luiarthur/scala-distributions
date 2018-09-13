package distribution.continuous

import distribution.UnivariateContinuous
import distribution.RandomGeneric
import org.apache.commons.math3.special.Gamma._
import org.apache.commons.math3.special.Beta._
import math.{log, exp}

case class Beta(a:Double, b:Double) extends UnivariateContinuous {
  require(a > 0 && b > 0, "In Beta(a, b): a, b > 0 required!")

  lazy val mean = a / (a + b)
  lazy val variance = mean * mean * b / (a * (a + b + 1))
  lazy val min = 0
  lazy val max = 1
  lazy val isMaxInclusive = false
  lazy val isMinInclusive = false
  lazy val mode = (a, b) match {
    case (1, 1) => 0.5 // in fact, any value in support
    case (aa, bb) if aa > 1 && bb > 1 => (a - 1) / (a + b - 2)
    case (1, bb) if bb > 1 => 0
    case (aa, 1) if aa > 1 => 1
    case _ => Double.NaN // not defined
  }

  override def lpdf(x:Double):Double = if (inSupport(x)) {
    logGamma(a + b) - logGamma(a) - logGamma(b) + (a - 1) * log(x) + (b - 1) * log(1 - x) 
  } else {
    Double.NegativeInfinity
  }

  def pdf(x:Double):Double = math.exp(lpdf(x))
  def cdfInSupport(x:Double): Double = regularizedBeta(x, a, b)

  override def toString = {
    s"Beta(a:$a, b:$b)"
  }

  def sample[Rng <: distribution.RandomGeneric](rng: Rng):Double = {
    rng.nextBeta(a, b)
  }

  // TODO: override quantile for case when mode is not defined
}
