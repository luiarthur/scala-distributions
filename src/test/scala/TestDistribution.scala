import org.scalatest.FunSuite
import distribution.helper.timer
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
    val d = new Binomial(n, p)

    // Assertions
    assertApprox(d.pdf(3), .3456)
    assertApprox(d.cdf(3), .66304)
  }
}

