package distribution.continuous

import distribution.UnivariateContinuous
import distribution.RandomGeneric
import org.apache.commons.math3.special.Erf.erf

case class Normal(mean:Double=0,sd:Double=1) extends UnivariateContinuous {

  require(sd > 0, "In Normal(mean, sd): sd > 0 required!")
  type RvType = Double

  lazy val min = Double.NegativeInfinity
  lazy val max = Double.PositiveInfinity
  lazy val isMaxInclusive = false
  lazy val isMinInclusive = false
  lazy val mode = mean

  lazy val variance = sd * sd

  override def lpdf(x:RvType):Double = {
    lazy val z = (x - mean) / sd
    -0.5 * math.log(2*math.Pi*sd*sd) - math.pow(z, 2) / 2
  }

  def pdf(x:RvType):Double = math.exp(lpdf(x))
  def cdfInSupport(x:RvType):Double = {
    lazy val z = (x - mean) / sd
    0.5 * (1 + erf(z / math.sqrt(2)))
  }

  override def toString = {
    s"Normal(mean:$mean, sd:$sd)"
  }

  def sample[Rng <: distribution.RandomGeneric](rng: Rng):RvType = {
    rng.nextGaussian(mean, sd)
  }
}
