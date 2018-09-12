package distribution.continuous

import distribution.UnivariateContinuous
import distribution.RandomGeneric
import org.apache.commons.math3.special.Erf.erf
import distribution.SpecialFunctions.{sigmoid, sech}

case class Logistic(params: (Double,Double)=(0,1)) extends UnivariateContinuous(params) {

  type RvType = Double


  def this(mean:Double, scale:Double) {
    this( (mean, scale) )
  }

  def inSupport(x:RvType) = true

  lazy val (mean, scale) = params
  require(scale > 0, "Logistic(mean,scale): scale > 0 required!")

  lazy val variance = math.pow(scale * math.Pi, 2) / 3

  lazy val min = Double.NegativeInfinity
  lazy val max = Double.PositiveInfinity
  lazy val mode = mean

  private def pdfStandardized(x:RvType):Double = {
    0.25 * math.pow(sech(x / 2), 2)
  }

  def pdf(x:RvType):Double = (mean, scale) match {
    case (0, 1) => pdfStandardized(x)
    case _ => pdfStandardized((x - mean) / scale) / scale
  }

  def cdf(x:RvType):Double = sigmoid((x - mean) / scale)

  override def toString = {
    s"Logistic(mean:$mean, scale:$scale)"
  }

  def sample[Rng <: distribution.RandomGeneric](rng: Rng):RvType = {
    rng.nextLogistic(mean, scale)
  }
}
