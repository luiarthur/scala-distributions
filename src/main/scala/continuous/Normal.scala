package distributions.continuous

import distributions.GenericDistribution

case class Normal(params: (Double,Double)) extends GenericDistribution(params) {
  type RvType = Double

  def this(mean:Double, sd:Double) {
    this( (mean, sd) )
  }

  val (mean, sd) = params

  override def lpdf(x:Double) = {
    val z = (x - mean) / sd
    -0.5 * math.log(2*math.Pi) - z*z / 2
  }

  def pdf(x:Double):Double = math.exp(lpdf(x))
  def cdf(x:Double):Double = ???

  override def toString = {
    s"Normal(mean:$mean, sd:$sd)"
  }

  def sample() = distributions.RandomPar.nextGaussian(mean, sd)
}
