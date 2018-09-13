import org.scalatest.FunSuite
import distribution.helper.{timer, timerWithTime}
import distribution.Univariate
import distribution.{RandomGeneric => RNG}
import org.apache.commons.math3.{distribution => apacheDist}

class UnivariateContinuousTester(dist:Univariate[Double], adist:apacheDist.RealDistribution, x:Double, rng:RNG, debug:Boolean=false, percentile:Double=.6, eps:Double=1E-6, n:Int=1E6.toInt) extends TestUtil{
  val samples = Vector.fill(n){ dist.sample(rng) }

  def testMean(eps:Double=1E-2) {
    val sampleMean:Double = mean(samples)
    val distMean:Double = dist.mean
    val adistMean:Double = adist.getNumericalMean

    assertApprox(sampleMean, distMean, eps, debug=debug)
    assertApprox(distMean, adistMean, eps, debug=debug)
  }

  def testVariance(eps:Double=1E-2) {
    val sampleVar:Double = variance(samples)
    val distVar:Double = dist.variance
    val adistVar:Double = adist.getNumericalVariance

    assertApprox(sampleVar, distVar, eps, debug=debug)
    assertApprox(distVar, adistVar, eps, debug=debug)
  }

  def testPdf(x:Double, eps:Double=1E-5) = {
    assertApprox(dist.pdf(x), adist.density(x), eps, debug=debug, msg="pdf")
  }

  def testCdf(x:Double, eps:Double=1E-5) = {
    assertApprox(dist.cdf(x), adist.cumulativeProbability(x), eps, debug=debug, msg="cdf")
  }

  def testQuantile(p:Double, eps:Double=1E-6) {
    assertApprox(dist.quantile(p), adist.inverseCumulativeProbability(p), eps=eps, debug=debug, msg="quantile")
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
    testVariance()
    testPdf(x)
    testCdf(x)
    testQuantile(percentile)
    testSpeed()
  }

}
