import org.scalatest.FunSuite
import distribution.helper.{timer, timerWithTime}
import org.apache.commons.math3.random.RandomDataGenerator


class TestDistribution extends TestUtil {

  test("Normal") {
    import distribution.continuous.Normal
    val d = new Normal(3.0, 2.0)
    // Assertions
    assertApprox(d.pdf(0.5), 0.09132454)
    assertApprox(d.lcdf(0.5), -2.247626, debug=true)

    // Speed test. These take much less than a second. So they're fine.
    //val idx = (0 until 1E6.toInt)
    //timer { print("normal pdf:  "); idx.foreach{ i =>(new Normal(3,2)).pdf(0.5) }}
    //timer { print("normal lpdf: "); idx.foreach{ i =>(new Normal(3,2)).lpdf(0.5) }}
    //timer { print("normal cdf:  "); idx.foreach{ i =>(new Normal(3,2)).cdf(0.5) }}
    //timer { print("normal lcdf: "); idx.foreach{ i =>(new Normal(3,2)).lcdf(0.5) }}
  }

  test("Bernoulli") {
    import distribution.discrete.Bernoulli
    val p = 0.6
    val d = new Bernoulli(p)
    // Assertions
    assert(d.pdf(0) == 1 - p)
    assert(d.pdf(1) == p)
    assert(d.pdf(-1) == 0)
    assert(d.pdf(2) == 0)
    assert(d.cdf(0) == 1 - p)
    assert(d.cdf(1) == 1)
    assert(d.cdf(-1) == 0)
    assert(d.cdf(2) == 1)
  }

  test("Binomial") {
    import distribution.discrete.Binomial
    val p = 0.6
    val n = 5
    val d = Binomial(n, p)

    // Assertions
    assertApprox(d.pdf(3), .3456)
    assertApprox(d.cdf(3), .66304)

    // Timings
    val idx = (0 until 1E6.toInt)
    val timeCutoff = .75

    // Done. Fast enough.
    //assert(timerWithTime{
    //  idx.foreach{i => Binomial(n,p).pdf(3)}
    //}._2 < timeCutoff)
  }

  testWithMsg("Negative Binomial") {
    import distribution.discrete.NegativeBinomial
    val p = 0.6
    val n = 5
    val d = NegativeBinomial(n, p)


    // Assertions
    assertApprox(d.pdf(3), .1741824, debug=true)
    assertApprox(d.cdf(3), .5940864, debug=true)

    // Done. Fast enough.
    /*
    val idx = (0 until 1E6.toInt)
    val timeCutoff = .75
    assert(timerWithTime{
      idx.foreach{i => NegativeBinomial(5,.6).pdf(3)}
    }._2 < timeCutoff)
    assert(timerWithTime{
      idx.foreach{i => NegativeBinomial(5,.6).lpdf(3)}
    }._2 < timeCutoff)
    assert(timerWithTime{
      idx.foreach{i => NegativeBinomial(5,.6).cdf(3)}
    }._2 < timeCutoff)
    */
  }

  testWithMsg("Poisson") {
    import distribution.discrete.Poisson
    val lam = 4
    val d = Poisson(lam)
    val x = 5

    // Assertions
    assertApprox(d.pdf(x), .1562935, debug=true)
    assertApprox(d.cdf(x), .7851304, debug=true)

    // Done. Fast enough.
    /*
    val idx = (0 until 1E6.toInt)
    val timeCutoff = .75
    assert(timerWithTime{ idx.foreach{i => Poisson(lam).pdf(x)} }._2 < timeCutoff)
    assert(timerWithTime{ idx.foreach{i => Poisson(lam).cdf(x)} }._2 < timeCutoff)
    */
  }

  testWithMsg("Multinomial") {
    import distribution.discrete.Multinomial
    val m = 10
    val J = 5
    lazy val probUnnormalized = Array.tabulate(J){ _.toDouble + 1}
    val prob = probUnnormalized.map{ _ / probUnnormalized.sum }
    val d = Multinomial(m, prob)

    val x = Array.range(0, J)

    // Assertions
    assertApprox(d.pdf(x), 0.01573224, debug=true)

    val idx = (0 until 1E6.toInt)
    val timeCutoff = .75
    //assert(timerWithTime{ idx.foreach{i => Poisson(lam).pdf(x)} }._2 < timeCutoff)
    //assert(timerWithTime{ idx.foreach{i => Poisson(lam).cdf(x)} }._2 < timeCutoff)
  }

  // Continuous Distributions
  testWithMsg("Gamma") {
    import distribution.continuous.Gamma
    val (a, b) = (5.0, 3.0)
    val d = Gamma(a, b)

    // Assertions
    assertApprox(d.pdf(2), .4015579, debug=true)
    assertApprox(d.cdf(2), .7149435, debug=true)
    assertApprox(d.mean, 1.66666666, debug=true)
    assertApprox(d.variance, 0.555555, debug=true)
  }

  testWithMsg("Inverse Gamma") {
    import distribution.continuous.InverseGamma
    val (a, b) = (3.0, 5.0)
    val d = InverseGamma(a, b)

    // Assertions
    assertApprox(d.pdf(4), 0.0699474601, debug=true)
    assertApprox(d.cdf(4), 0.8684676654, debug=true)
    assertApprox(d.mean, 2.5, debug=true)
    assertApprox(d.variance, 6.25, debug=true)
  }

  testWithMsg("Beta") {
    import distribution.continuous.Beta
    val (a, b) = (3.0, 5.0)
    val d = Beta(a, b)

    // Assertions
    assertApprox(d.pdf(.6), 0.96768, debug=true)
    assertApprox(d.cdf(.6), 0.90374, debug=true)
    assertApprox(d.mean, 0.375, debug=true)
    assertApprox(d.variance, 0.0260416, debug=true)
  }

  testWithMsg("Logistic") {
    import distribution.continuous.Logistic
    val (m, s) = (3.0, 2.0)
    val d = Logistic(m, s)

    // Assertions
    assertApprox(d.pdf(4), .117501856, debug=true)
    assertApprox(d.cdf(4), .62245933120, debug=true)
    assertApprox(d.mean, 3, debug=true)
    assertApprox(d.variance, 13.1594725, debug=true)
  }
}

