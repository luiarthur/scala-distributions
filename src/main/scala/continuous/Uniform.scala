package distribution.continuous

import distribution.UnivariateContinuous
import distribution.RandomGeneric
import math.{log, exp}

case class Uniform(a:Double, b:Double) extends UnivariateContinuous {
  require(b > a, "Uniform(a, b): b > a required!")

  type RvType = Double

  lazy val mean:Double = (a + b) / 2
  lazy val variance:Double = math.pow(b - a, 2) / 12.0
  lazy val range:Double = b - a
  private lazy val preComputedPdf = 1.0 / range
  private lazy val preComputedLogPdf = -math.log(range)

  lazy val min = a
  lazy val max = b
  lazy val isMinInclusive = true
  lazy val isMaxInclusive = true
  lazy val mode = (a + b) / 2


  override def lpdf(x:Double):Double = if (inSupport(x)) {
    preComputedLogPdf
  } else {
    Double.NegativeInfinity
  }

  def pdf(x:Double):Double = preComputedPdf
  def cdfInSupport(x:Double):Double = {
    (x - a) / range
  }

  override def toString = {
    s"Beta(a:$a, b:$b)"
  }

  def sample[Rng <: distribution.RandomGeneric](rng: Rng):Double = {
    rng.nextUniform(a, b)
  }
}
