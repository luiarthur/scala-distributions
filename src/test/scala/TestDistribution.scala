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

    assert(timerWithTime{
      idx.foreach{i => Binomial(n,p).pdf(3)}
    }._2 < timeCutoff)
  }

  testWithMsg("Negative Binomial") {
    import distribution.discrete.NegativeBinomial
    val p = 0.6
    val n = 5
    val d = NegativeBinomial(n, p)


    // Assertions
    assertApprox(d.pdf(3), .1741824, debug=true)
    assertApprox(d.cdf(3), .5940864, debug=true)

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
  }

  testWithMsg("Poisson") {
    import distribution.discrete.Poisson
    val lam = 4
    val d = Poisson(lam)
    val x = 5

    // Assertions
    assertApprox(d.pdf(x), .1562935, debug=true)
    assertApprox(d.cdf(x), .7851304, debug=true)

    val idx = (0 until 1E6.toInt)
    val timeCutoff = .75
    assert(timerWithTime{ idx.foreach{i => Poisson(lam).pdf(x)} }._2 < timeCutoff)
    assert(timerWithTime{ idx.foreach{i => Poisson(lam).cdf(x)} }._2 < timeCutoff)
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

}

