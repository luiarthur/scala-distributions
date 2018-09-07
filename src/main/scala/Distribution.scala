package distribution

abstract class Distribution[RvType](params: Any*) {
  type meanType
  type varType

  val mean: meanType
  val variance: varType

  def sample[Rng <: distribution.RandomGeneric](rng:Rng): RvType
  def pdf(x:RvType): Double
  def cdf(x:RvType): Double
  def inSupport(x:RvType): Boolean
  def ccdf(x:RvType): Double = 1 - cdf(x)
  def lpdf(x:RvType): Double = math.log(pdf(x))
  def lcdf(x:RvType): Double = math.log(cdf(x))
  def lccdf(x:RvType): Double = math.log(ccdf(x))

  //def samples(n:Int):Vector[RvType] = Vector.tabulate(n){ i => sample }
}


abstract class Univariate[RvType](params: Any*) extends Distribution[RvType] {
  type meanType = Double
  type varType = Double

  def quantile(p:Double): RvType = {
    require(p >= 0 && p <= 1)
    ???
  }

  def max(): RvType = ???
  def min(): RvType = ???
}
