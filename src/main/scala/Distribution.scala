package distribution

abstract class Distribution[RvType] {
  type meanType
  type varType

  // TODO: Make these lazy val
  def mean: meanType
  def variance: varType

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


abstract class Univariate[RvType] extends Distribution[RvType] {
  type meanType = Double
  type varType = Double

  // TODO: Refactor. Remove ??? and implement in each distribution.
  def quantile(p:Double, eps:Double=1E-12, maxIter:Int=10000, verbose:Int=1): Double
  def max: Double
  def min: Double
  def isMaxInclusive: Boolean
  def isMinInclusive: Boolean
  def mode: RvType
  protected def cdfInSupport(x:RvType): Double
}


abstract class UnivariateContinuous extends Univariate[Double] {
  def inSupport(x:Double): Boolean = {
    (min < x && x < max) || (isMaxInclusive && x == max) || (isMinInclusive && x == min)
  }

  def cdf(x:Double):Double = x match {
    case v if inSupport(v) => cdfInSupport(x)
    case v if v <= min => 0
    case v if v >= max => 1
  }

  def quantile(p: Double, eps:Double=1E-12, maxIter:Int=10000, verbose:Int=1): Double = {
    require(0 <= p && p <= 1, "quantile(p, eps): 0 <= p <= 1 required!")
    quantileNewton(p=p, init=mode, eps=eps, maxIter=maxIter, verbose=verbose)
  }

  /* See: http://www.statsci.org/smyth/pubs/qinvgaussPreprint.pdf
   *
   * Basic idea:
   * - Solve for quantile using Newton's method. 
   * - Use the mode as initial value for monotonic convergence. (This is desireable and efficient.)
   * - This will converge for unimodal continuous distributions.
   * - For continuous distributions that are not unimodal (perhaps for some parameterizations), 
   *   finding the quantile of transformations that yield unimodal distributions and then back-transforming
   *   the resulting quantile will yield the desired result.
   * - For example, Gamma(shape < 1, rate = 2) has mode at 0, but is not in support. Find the quantile of
   *   the log transformed variable. Then exponentiate the result.
   */
  protected def quantileNewton(p:Double, init:Double, eps:Double, maxIter:Int, verbose:Int): Double = {
    // Function that I want to find the zero for.
    def f(x:Double) = cdf(x) - p

    // Derivative of that function.
    def fPrime(x:Double) = pdf(x)

    p match {
      case 0 => min
      case 1 => max
      case _ => helper.newton(init, f, fPrime, eps=eps, maxIter=maxIter, verbose=verbose)
    }
  }
}


abstract class UnivariateDiscrete extends Univariate[Int] {
  def inSupport(x:Int): Boolean = {
    (min < x && x < max) || (isMaxInclusive && x == max) || (isMinInclusive && x == min)
  }

  def cdf(x:Int):Double = x match {
    case v if inSupport(v) => cdfInSupport(x)
    case v if v <= min => 0
    case v if v >= max => 1
  }

  def quantile(p: Double, eps:Double=1E-12, maxIter:Int=10000, verbose:Int=1): Double = {
    require(0 <= p && p <= 1, "quantile(p, eps): 0 <= p <= 1 required!")
    def engine(x: Int, _cdf:Double=0): Double = {
      val _newCdf = _cdf + pdf(x.toInt)
      if (_newCdf >= p) x else engine(x + 1, _newCdf)
    }

    p match {
      case 0 => min
      case 1 => max
      case _ => engine(min.toInt)
    }
  }
}

