import org.scalatest.FunSuite
import distribution.helper.{timer, timerWithTime}
import distribution.Univariate
import distribution.{RandomGeneric => RNG}

case class UnivariateTruth(mean:Double, variance:Double,
                           pdf:Double, cdf:Double, quantile:Double=0)

class UnivariateTester(dist:Univariate[Double], x:Double, rng:RNG, truth:UnivariateTruth, eps:Double=1E-2, n:Int=1E6.toInt, debug:Boolean=false) extends TestUtil {
  val samples = Vector.fill(n){ dist.sample(rng) }

  def testMean() {
    val sampleMean:Double = mean(samples)
    val distMean:Double = dist.mean
    assertApprox(sampleMean, distMean, eps, debug=debug)
    assertApprox(distMean, truth.mean, eps, debug=debug)
  }

  def testVar() = {
    val sampleVar:Double = variance(samples)
    val distVar:Double = dist.variance
    assertApprox(distVar, truth.variance, eps, debug=debug)
  }

  def testPdf(x:Double, truthPdf:Double) = {
    if (debug) print("pdf: ")
    assertApprox(dist.pdf(x), truthPdf, eps, debug=debug)
  }

  def testCdf(x:Double, truthCdf:Double) = {
    if (debug) print("cdf: ")
    assertApprox(dist.cdf(x), truthCdf, eps, debug=debug)
  }

  def testQuantiles(eps:Double=1E-3) {
  }

  def testSpeed(maxTimeout:Double=1.0) {
    val n = 1E6.toInt
    val idx = (0 until n)
    val (_, timing) = timerWithTime{
      idx.foreach{ _ => dist.sample(rng) }
    }
    assert(timing < 1.0)
  }

  def test():Unit = {
    testMean()
    testVar()
    testPdf(x, truth.pdf)
    testCdf(x, truth.cdf)
    testQuantiles()
    testSpeed()
  }
}
