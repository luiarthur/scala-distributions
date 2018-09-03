package distribution.continuous

import distribution.Distribution
import distribution.RandomGeneric

case class Normal(params: (Double,Double)) extends Distribution(params) {
  type RvType = Double

  def this(mean:Double, sd:Double) {
    this( (mean, sd) )
  }

  val (mean, sd) = params

  override def lpdf(x:Double) = (mean, sd) match {
    case (0, 1) => -0.5 * math.log(2*math.Pi) - math.pow((x - mean) / sd, 2) / 2
  }

  def pdf(x:Double):Double = math.exp(lpdf(x))
  def cdf(x:Double):Double = ???

  override def toString = {
    s"Normal(mean:$mean, sd:$sd)"
  }

  def sample[Rng <: distribution.RandomGeneric](rng: Rng):Double = {
    rng.nextGaussian(mean, sd)
  }
}
