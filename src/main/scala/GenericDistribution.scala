package distribution

abstract class GenericDistribution(params: Any*) {
  type RvType 
  def sample: RvType
  def pdf(x:RvType): Double
  def cdf(x:RvType): Double
  def ccdf(x:RvType): Double = 1 - cdf(x)
  def lpdf(x:RvType): Double = math.log(pdf(x))
  def lcdf(x:RvType): Double = math.log(cdf(x))
  def lccdf(x:RvType): Double = math.log(ccdf(x))

  //def samples(n:Int):Vector[RvType] = Vector.tabulate(n){ i => sample }
}

