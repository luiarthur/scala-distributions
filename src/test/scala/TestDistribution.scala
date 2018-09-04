import org.scalatest.FunSuite
import distribution.helper.timer
import org.apache.commons.math3.random.RandomDataGenerator


class TestDistribution extends FunSuite {
  def assertApprox(x:Double, y:Double, eps:Double=.1E-4, debug:Boolean=false) = {
    val valid = math.abs(x - y) < eps
    if (debug) {
      if (!valid) {
        println(s"invalid -- x: $x; y:$y")
      } else println(s"valid -- x: $x; y:$y")
    }
    assert(valid)
  }

  def testWithMsg[R](msg:String)(block: => R) = {
    test(msg) {
      print(msg + " -- ")
      timer {
        block
      }
    }
  }

  test("Normal") {
    import distribution.continuous.Normal
    val n = new Normal(3.0, 2.0)
    // TODO: timer
    assertApprox(n.pdf(0.5), 0.09132454)
    assertApprox(n.lcdf(0.5), -2.247626, debug=true)
  }
}

