import org.scalatest.FunSuite
import distribution.helper.timer
import org.apache.commons.math3.random.RandomDataGenerator


class TestDistribution extends TestUtil {

  test("Normal") {
    import distribution.continuous.Normal
    val n = new Normal(3.0, 2.0)
    // Assertions
    assertApprox(n.pdf(0.5), 0.09132454)
    assertApprox(n.lcdf(0.5), -2.247626, debug=true)

    // Speed test. These take much less than a second. So they're fine.
    //val idx = (0 until 1E6.toInt)
    //timer { print("normal pdf:  "); idx.foreach{ i =>(new Normal(3,2)).pdf(0.5) }}
    //timer { print("normal lpdf: "); idx.foreach{ i =>(new Normal(3,2)).lpdf(0.5) }}
    //timer { print("normal cdf:  "); idx.foreach{ i =>(new Normal(3,2)).cdf(0.5) }}
    //timer { print("normal lcdf: "); idx.foreach{ i =>(new Normal(3,2)).lcdf(0.5) }}
  }
}

