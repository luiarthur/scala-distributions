import org.scalatest.FunSuite
import distribution.helper.{timer, timerWithTime}
import org.apache.commons.math3.random.RandomDataGenerator
import distribution.continuous._
import distribution.discrete._
import distribution.{RandomSeq, RandomPar}


class TestDistribution2 extends TestUtil {
  // Continuous
  test("Normal2") {
    val rng = new RandomSeq(new scala.util.Random(0))
    val (mu, sig) = (3, 4)
    val x = 2.0
    val p = 0.7
    val truth = UnivariateTruth(mu, sig*sig, pdf=0.09666703, cdf=0.4012937, quantile=5.097602050832163)
    val tester = new UnivariateTester(Normal(mu,sig), x=x, p=p, rng=rng, truth=truth, debug=true)

    tester.test
  }

  test("Gamma2") {
    val rng = new RandomSeq(new scala.util.Random(0))
    val (a, b) = (5.0, 3.0)
    val x = 2.0
    val p = 0.7
    val pdf = 0.4015579
    val cdf = 0.7149435
    val quantile = 1.9634537712323352
    val truth = UnivariateTruth(a/b, a/(b*b), pdf=pdf, cdf=cdf, quantile=quantile)
    val tester = (new UnivariateTester(Gamma(a,b), x=x, rng=rng, p=p, truth=truth, debug=true))
    tester.testPdf(0, 0)
    tester.testCdf(0, 0)
    tester.testCdf(-1, 0)
    tester.test()
    assertApprox(Gamma(.3, 3).quantile(.7), 0.085521637773684, eps=1E-10)
  }

  test("InverseGamma2") {
    val rng = new RandomSeq(new scala.util.Random(0))
    val (a, b) = (3.0, 5.0)
    val x = 4.0
    val pdf = 0.0699474601
    val cdf = 0.8684676654
    val mean = 2.5
    val variance = 6.25
    val q = 2.188110164362591
    val p = .6
    val truth = UnivariateTruth(mean=mean, variance=variance, pdf=pdf, cdf=cdf, quantile=q)
    val tester = (new UnivariateTester(InverseGamma(a,b), x=x, p=p, rng=rng, truth=truth, debug=true))
    tester.testPdf(0, 0)
    tester.testCdf(0, 0)
    tester.testCdf(-1, 0)
    tester.test()
  }

  test("Beta2") {
    val rng = new RandomSeq(new scala.util.Random(0))
    val (a, b) = (3.0, 5.0)
    val x = 0.6
    val pdf = 0.96768
    val cdf = 0.90374
    val mean = a / (a + b)
    val variance = 0.0260416
    val q = 0.4092151219095549
    val p = .6
    val truth = UnivariateTruth(mean=mean, variance=variance, pdf=pdf, cdf=cdf, quantile=q)
    val tester = new UnivariateTester(Beta(a,b), x=x, p=p, rng=rng, truth=truth, debug=true)
    tester.testPdf(0, 0)
    tester.testPdf(1, 0)
    tester.testCdf(0, 0)
    tester.testCdf(-1, 0)
    tester.testCdf(1, 1)
    tester.testCdf(2, 1)
    tester.test()
  }

  test("Logistic2") {
    val rng = new RandomSeq(new scala.util.Random(0))
    val (a, b) = (3.0, 2.0)
    val x = 4.0
    val pdf = .117501856
    val cdf = .622459331
    val mean = a
    val variance = 13.1594725
    val p = .6
    val q = 3.8109302162163283
    val truth = UnivariateTruth(mean=mean, variance=variance, pdf=pdf, cdf=cdf, quantile=q)
    val tester = new UnivariateTester(Logistic(a,b), x=x, p=p, rng=rng, truth=truth, debug=true)
    tester.test()
  }

  test("Uniform2") {
    val rng = new RandomSeq(new scala.util.Random(0))
    val (a, b) = (2.0, 5.0)
    val x = 4.0
    val pdf = 1 / 3.0
    val cdf = 2.0 / 3.0
    val mean = 3.5
    val variance = .75
    val p = .6
    val q = 3.8
    val truth = UnivariateTruth(mean=mean, variance=variance, pdf=pdf, cdf=cdf, quantile=q)
    val tester = new UnivariateTester(Uniform(a,b), x=x, p=p, rng=rng, truth=truth, debug=true)
    tester.test()

    tester.testPdf(a, pdf)
    tester.testPdf(b, pdf)
    tester.testCdf(a, 0)
    tester.testCdf(b, 1)
    tester.testCdf(a-1E-10, 0)
    tester.testCdf(b+1E-10, 1)
  }

  // Discrete
  test("Bernoulli") {
    val rng = new RandomSeq(new scala.util.Random(0))
    val p = .6
    (0 to 10).foreach{ i =>
      val q = i / 10.0
      assert(Bernoulli(p).quantile(q) == (if (q > 1 - p) 1 else 0))
    }
  }

  test("Binomial") {
    val rng = new RandomSeq(new scala.util.Random(0))
    assert(Binomial(10, .6).quantile(0.0) == 0)
    assert(Binomial(10, .6).quantile(0.2) == 5)
    assert(Binomial(10, .6).quantile(0.4) == 6)
    assert(Binomial(10, .6).quantile(0.6) == 6)
    assert(Binomial(10, .6).quantile(0.8) == 7)
    assert(Binomial(10, .6).quantile(1.0) == 10)
  }

  test("Negative Binomial") {
    val rng = new RandomSeq(new scala.util.Random(0))
    assert(NegativeBinomial(10, .6).quantile(.7) == 8)
    assert(NegativeBinomial(10, .6).quantile(0) == 0)
    assert(NegativeBinomial(10, .6).quantile(1) == Double.PositiveInfinity)
    assert(NegativeBinomial(10, .6).quantile(.2) == 4)
    assert(NegativeBinomial(10, .6).quantile(.4) == 5)
    assert(NegativeBinomial(10, .6).quantile(.6) == 7)
    assert(NegativeBinomial(10, .6).quantile(.8) == 9)
  }

  test("Poisson") {
    val rng = new RandomSeq(new scala.util.Random(0))
    assert(Poisson(7).quantile(.7) == 8)
    assert(Poisson(7).quantile(0) == 0)
    assert(Poisson(7).quantile(1) == Double.PositiveInfinity)

    assert(Poisson(7).quantile(.2) == 5)
    assert(Poisson(7).quantile(.4) == 6)
    assert(Poisson(7).quantile(.6) == 8)
    assert(Poisson(7).quantile(.8) == 9)
  }

  // Multivariate Continuous

  // Multivariate Discrete
}


