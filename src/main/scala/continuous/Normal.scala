package distribution.continuous

import distribution.Univariate
import distribution.RandomGeneric
import org.apache.commons.math3.special.Erf.erf

case class Normal(params: (Double,Double)=(0,1)) extends Univariate(params) {

  type RvType = Double

  def inSupport(x:Double) = true

  def this(mean:Double, sd:Double) {
    this( (mean, sd) )
  }

  val (mean, sd) = params
  //require(sd > 0, "In Normal(mean, sd): sd > 0 required!")

  val variance = sd * sd

  override def lpdf(x:Double):Double = {
    lazy val z = (x - mean) / sd
    -0.5 * math.log(2*math.Pi*sd*sd) - math.pow(z, 2) / 2
  }

  def pdf(x:Double):Double = math.exp(lpdf(x))
  def cdf(x:Double):Double = {
    lazy val z = (x - mean) / sd
    0.5 * (1 + erf(z / math.sqrt(2)))
  }

  override def toString = {
    s"Normal(mean:$mean, sd:$sd)"
  }

  def sample[Rng <: distribution.RandomGeneric](rng: Rng):Double = {
    rng.nextGaussian(mean, sd)
  }
}
