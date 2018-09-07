import org.scalatest.FunSuite
import distribution.helper.timer
import distribution.Univariate
import distribution.{RandomGeneric => RNG}

case class UnivariateTruth(mean:Double, variance:Double,
                           pdf:Double, cdf:Double, quantile:Double)

class UnivariateTester(dist:Univariate, n:Int=1E6.toInt, rng:RNG, truth:UnivariateTruth) extends TestUtil {
  private val rawSamples = Vector.fill(n){ dist.sample(rng) }

  private val samples = rawSamples match {
    case s: Vector[Double] => s
    case s: Vector[Int] => s.map{ si => si.toDouble }
    case _ => Vector[Double]()
  }


  def testMean(eps:Double = 1E-3) {
    val sampleMean:Double = mean(samples)
    val distMean:Double = dist.mean
    assertApprox(sampleMean, distMean, eps)
    assertApprox(distMean, truth.mean, eps)
  }

  def testVar(eps:Double=1E-3) = {
    val sampleVar:Double = variance(samples)
    val distVar:Double = dist.variance
    assertApprox(distVar, truth.variance, eps)
  }

  def testPdf() = {
  //  assertApprox(dist.pdf(x), truth.pdf, eps)
  }

  def testCdf() = {
  //  assertApprox(dist.cdf(x), truth.pdf, eps)
  }

  def testQuantiles(eps:Double=1E-3) {
  }

  def testSpeed(maxTimeout:Double=1.0) {
  }

  def test():Unit = {
    testMean()
    testVar()
    testPdf()
    testCdf()
    testQuantiles()
    testSpeed()
  }
}
