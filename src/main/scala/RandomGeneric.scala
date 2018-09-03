package distribution
import scala.annotation.tailrec

/* Notes:
 * What I am learning from this exercise is that you really only need a random uniform
 * to generate other random variables.
 * From a uniform, you can generate Normals using box-mueller,
 * then you can sample several other distributions.
 * The other critical distribution is the gamma, which from there, you can easily
 * sample some distributions.
 * For discrete distributions, Poisson presents most challenges. From Poisson
 * (and gamma), though, you can get negative binomial (and geometric).
 *
 * In conclusion, given that you have a random unit uniform sampler, implement these first:
 * - normal
 * - gamma
 * - poisson
 * The other distributions will transformations of (an assortment of) these and the uniform.
 *
 * Then, the two most important multivariate distributions are 
 *   - multivariate Normal
 *   - Dirichlet
 *
 * For discrete, you also need a (weighted) categorical sampler.
 *
 * See common distributions to implement here:
 * http://commons.apache.org/proper/commons-math/javadocs/api-3.6/org/apache/commons/math3/random/RandomDataGenerator.html
 */


trait RandomGeneric {
  import SpecialFunctions.logFactorial // for rpois
  import SpecialFunctions.{vvAdd, mvMult, choleskyL} // for rmvnorm

  // Methods to Implement
  def setSeed(seed:Long): Unit
  def nextBoolean(): Boolean
  def nextInt(n:Int): Int
  def nextLong(): Long
  def nextFloat(): Float
  def nextDouble(): Double

  // Methods to inherit

  // Univariate Continuous
  def nextGaussian(mean:Double=0, sd:Double=1): Double = {
    require(sd > 0, "In nextGaussian(mean, sd): sd needs to be positive!")

    (mean, sd) match {
      // Standard Normal. box-meuller.
      case (0, 1) =>
        math.sqrt(-2 * math.log(nextDouble)) * math.cos(2 * math.Pi * nextDouble)
      case _ => mean + nextGaussian(0, 1) * sd
    }
  }

  def nextUniform(a:Double=0, b:Double=1): Double = {
    require(a <= b, "In nextUniform(a, b): a <= b required!")
    (a, b) match {
      case (0, 1) => nextDouble()
      case _ => a + nextDouble() * (b - a)
    }
  }

  def nextExponential(rate:Double): Double = {
    require(rate > 0)
    -math.log(1 - nextDouble()) / rate
  }

  // next Gamma, where rate == 1, shape >= 1.
  private def nextGammaRateIsOneShapeGeOne(shape:Double): Double = {
    val d = shape - 1.0 / 3.0
    val c = 1.0 / math.sqrt(9*d)

    def engine(): Double = {
      val z = nextGaussian()
      lazy val u = nextDouble()
      lazy val v = math.pow(1 + c*z, 3)
      if (z > -1.0/c && math.log(u) < z*z/2.0 + d*(1-v+math.log(v))) {
        d * v
      } else engine()
    }

    engine()
  }

  def nextGamma(shape:Double, rate:Double):Double = {
    require(shape > 0 && rate > 0)
    if (shape >= 1) {
      nextGammaRateIsOneShapeGeOne(shape) / rate
    } else {
      nextGamma(shape+1, rate) * math.pow(nextDouble(), 1 / shape)
    }
  }

  def nextInverseGamma(shape:Double, rate:Double): Double = {
    require(shape > 0 && rate > 0, "In nextInverseGamma(shape, rate): shape > 0 & rate > 0 required!")
    1 / nextGamma(shape, rate)
  }

  def nextBeta(a:Double, b:Double): Double = {
    require(a > 0 && b > 0, "In nextBeta(a, b): a > 0 & b > 0 required!")
    lazy val x = nextGamma(a, 1)
    lazy val y = nextGamma(b, 1)
    x / (x + y)
  }

  def nextChisq(nu:Double): Double = {
    require(nu > 0, "In nextChisq(nu): nu > 0 required!")
    nextGamma(nu/2, 0.5)
  }

  def nextT(df:Double): Double = {
    require(df > 0, "In nextT(df): df > 0 required!")
    nextGaussian() * math.sqrt(df / nextChisq(df))
  }

  def nextF(df1:Double, df2:Double): Double = {
    require(df1 > 0 && df2 > 0, "In nextF(df1, df2): df1 > 0 & df2 > 0 required!")
    (nextChisq(df1) / df1) / (nextChisq(df2) / df2)
  }

  def nextWeibull(shape:Double, scale:Double): Double = {
    require(shape > 0 && scale > 0, "In nextWeibull(shape, scale): shape > 0 & scale > 0 required!")
    scale * math.pow(-math.log(nextDouble), 1 / shape)
  }

  // TODO: test
  def nextLogistic(mean:Double=0, scale:Double=1): Double = {
    require(scale > 0, "In nextLogistic: scale > 0 required!")
    lazy val u = nextDouble()

    (mean, scale) match {
      case (0, 1) => math.log(u) - math.log(1 - u)
      case _ => mean + scale * nextLogistic(0, 1)
    }
  }


  // Univariate Discrete
  def nextBernoulli(p: Double): Int = {
    require(p >= 0 && p <= 1)
    if (p > nextDouble()) 1 else 0
  }

  def nextBinomial(n:Int, p:Double):Int = {
    require(n >= 0 && p >= 0 && p <= 1)
    List.fill(n)(nextBernoulli(p)).sum
  }

  private def nextPoissonKnuth(lam:Double):Int = {
    val L = math.exp(-lam)

    @tailrec
    def engine(k:Int=0, p:Double=1): Int = {
      if (p > L) {
        engine(k+1, p * nextDouble()) 
      } else k-1
    }

    engine()
  }

  private def nextPoissonAccRej(lam:Double):Int = {
    val c = 0.767 - 3.36 / lam
    val beta = math.Pi / math.sqrt(3.0 * lam)
    val alpha = beta * lam
    val k = math.log(c) - lam - math.log(beta)

    @tailrec
    def engine(): Int = {
      val u = nextDouble()
      val x = (alpha - math.log(1 / u - 1)) / beta
      val n = math.floor(x).toInt
      if (n < 0) engine() else {
        val v = nextDouble()
        val y = alpha - beta * x
        val lhs = y + math.log(v) - 2 * math.log(1 + math.exp(y))
        val rhs = k + n * math.log(lam) - logFactorial(n)
        if (lhs <= rhs) n.toInt else engine()
      }
    }
    engine()
  }

  def nextPoisson(lam:Double):Int = {
    //https://www.johndcook.com/blog/2010/06/14/generating-poisson-random-values/
    require(lam > 0, "In nextPoisson(lam): lam > 0 required!")
    if (lam < 30) nextPoissonKnuth(lam) else nextPoissonAccRej(lam)
  }

  def nextNegativeBinomial(numSuccess:Double, probSuccess:Double):Int = {
    // numSuccess: target for number of successful trials (>0)
    // mean: numSuccess * (1-probSuccess) / probSuccess
    // samples: number of failures that occur before `r` successes are reached
    // TODO: Test using bernoulli trials!
    require(numSuccess >= 0 && probSuccess >= 0 && probSuccess <= 1,
            "In nextNegativeBinomial(numSuccess, probSuccess): numSuccess >= 0 && probSuccess >= 0 && probSuccess <= 1 required!")
    nextPoisson(nextGamma(shape=numSuccess, rate=probSuccess/(1-probSuccess)))
  }

  def nextGeom(r:Int, p:Double):Int = {
    nextNegativeBinomial(1, p)
  }

  def wsampleIndex(prob:IndexedSeq[Double], check:Boolean=true): Int = {
    if (check) prob.foreach{ p => require(p >= 0) }

    val pSum = prob.sum
    val u = nextDouble() * pSum

    def engine(cumsum:Double=prob.head, p:IndexedSeq[Double]=prob.tail, i:Int = 0):Int = {
      if (cumsum < u) {
        engine(cumsum + p.head, p.tail, i+1)
      } else {
        i
      }
    }

    engine()
  }

  def wsampleIndexByLogProb(logProb:IndexedSeq[Double]): Int = {
    val logProbMax = logProb.max
    val prob = logProb.map(_ - logProbMax)
    wsampleIndex(prob)
  }


  // Multivariate Continuous
  def nextDirichlet(a:IndexedSeq[Double]): IndexedSeq[Double] = {
    lazy val x = a.map{ai => nextGamma(ai, 1)}
    lazy val xSum = x.sum
    x.map{_ / xSum}
  }

  /* Commons Math version. (1.75x speed slowdown from Personal Version1)
  import org.apache.commons.math3.linear.{
    Array2DRowRealMatrix, ArrayRealVector, CholeskyDecomposition
  }
  def rmvnorm(m:Array[Double], cov:Array[Array[Double]]): Array[Double] = {
    val n = m.size
    val mVec = new Array2DRowRealMatrix(m)
    val z = new Array2DRowRealMatrix(Array.fill(n){ Rand.nextGaussian })
    val covMat = new Array2DRowRealMatrix(cov)
    val A = (new CholeskyDecomposition(covMat)).getL
    A.multiply(z).add(mVec).getData.flatten
  }
  */

  // Personal Version 
  /* @param m: mean
   * @param cov: covariance matrix
   * @return sample from multivariate normal
   */
  def nextMvNormal(m:Array[Double], cov:Array[Array[Double]]): Array[Double] = {
    /* Basic idea:
     *
     * LL' = covMat
     * z ~ N(0, I)
     * x = m + L*z
     */

    // Version1
    lazy val n = m.size
    lazy val z = Array.fill(n){nextGaussian()}
    lazy val L = choleskyL(cov)
    vvAdd(m, mvMult(L, z))

    // Version2 (twice as slow as version 1)
    //Linalg.rmvnorm(m, cov)

    // Version3 (way slow)
    //val n = m.size
    //val L = choleskyL(cov)
    //Array.tabulate(n){ i => {
    //  def engine(j:Int=0, s:Double=0): Double = j match {
    //    case j if j == i + 1 => s
    //    case j => engine(j + 1, L(i)(j) * Rand.nextGaussian + s)
    //  }
    //  engine() + m(i) 
    //}
  }

  // Multivariate Discrete
  def nextMultinomial(m: Int, prob: Array[Double], check:Boolean=true): Array[Int] = {
    require(m >= 0, "In nextMultinomial(m, p): m >= 0 required!")
    if (check) prob.foreach{ p => require(p >= 0) }

    val dim = prob.length
    val draw = Array.ofDim[Int](dim)
    (0 until m).foreach{ i =>
      val idx = wsampleIndex(prob)
      draw(idx) += 1
    }

    draw
  }
}

