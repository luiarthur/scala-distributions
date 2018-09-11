package distribution

abstract class Distribution[RvType](params: Any*) {
  type meanType
  type varType

  // TODO: Make these lazy val
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

  def quantile(p:Double, eps:Double=1E-12, maxIter:Int=10000): RvType = ???

  // TODO: Implement these
  lazy val max: Double = ???
  lazy val min: Double = ???
  lazy val mode: Double = ???
}

abstract class UnivariateContinuous(params: Any*) extends Univariate[Double] {
  override def quantile(p: Double, eps:Double=1E-12, maxIter:Int=10000): Double = {
    require(0 <= p && p <= 1, "quantile(p, eps): 0 <= p <= 1 required!")
    quantileNewton(p, eps, maxIter)
  }

  /* See: http://www.statsci.org/smyth/pubs/qinvgaussPreprint.pdf
   */
  private def quantileNewton(p:Double, eps:Double, maxIter:Int): Double = {
    def engine(current:Double, iter:Int):Double = {
      val next = current + (p - cdf(current)) / pdf(current)
      if (iter <= 0) {
        println(s"Not converged after ${maxIter} iterations!")
        next
      } else if (math.abs(next - current) < eps) {
        //println(s"Converged in ${maxIter - iter} iterations!")
        next 
      } else { 
        engine(next, iter-1)
      }
    }

    p match {
      case 0 => min
      case 1 => max
      case _ => engine(mode, iter=maxIter)
    }
  }
}

