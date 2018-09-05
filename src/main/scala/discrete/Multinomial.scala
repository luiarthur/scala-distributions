package distribution.discrete

import distribution.Distribution
import distribution.RandomGeneric
import distribution.SpecialFunctions._

// TODO: Test
case class Multinomial(params: (Int,Array[Double])) extends Distribution(params) {
  type RvType = Array[Int]
  type meanType = Array[Double]
  type varType = Array[Array[Double]]

  val (m, prob) = params
  //require???

  val mean = ???
  val variance = ???

  override def lpdf(x:RvType): Double = {
    ???
  }

  def pdf(x:RvType):Double = {
    math.exp(lpdf(x))
  }

  def cdf(x:RvType): Double = ???

  def sample[Rng <: RandomGeneric](rng:Rng): RvType = {
    rng.nextMultinomial(m, prob)
  }
}
