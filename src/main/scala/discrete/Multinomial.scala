package distribution.discrete

import distribution.Distribution
import distribution.RandomGeneric
import distribution.SpecialFunctions._

// TODO: Test
case class Multinomial(m:Int, prob:Array[Double]) extends Distribution[Array[Int]] {
  require(m > 0 && prob.forall(_ > 0))

  type RvType = Array[Int]
  type meanType = Array[Double]
  type varType = Array[Array[Double]]


  val mean = prob.map{ _ * m }
  val K = prob.size
  val variance = Array.tabulate(K,K){ case (i,j) => 
    if (i == j) m * prob(i) * (1 - prob(i)) else -m * prob(i) * prob(j)
  }

  override def lpdf(x:RvType): Double = {
    logFactorial(m) - x.map{logFactorial}.sum + x.zip(prob).map{
      case (xj, pj) => xj * math.log(pj)
    }.sum
  }

  def pdf(x:RvType):Double = {
    math.exp(lpdf(x))
  }

  def inSupport(x:RvType) = ???
  def cdf(x:RvType): Double = ???

  def sample[Rng <: RandomGeneric](rng:Rng): RvType = {
    rng.nextMultinomial(m, prob)
  }
}
