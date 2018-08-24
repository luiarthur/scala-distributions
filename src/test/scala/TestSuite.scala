import org.scalatest.FunSuite
import distributions.helper.timer
import org.apache.commons.math3.random.RandomDataGenerator

class MyFunSuite extends FunSuite {
  def testWithMsg[R](msg:String)(block: => R) = {
    test(msg) {
      print(msg + " -- ")
      timer {
        block
      }
    }
  }
}

class TestSuite extends MyFunSuite {
  import distributions.Random._
  val commonsMathR = new RandomDataGenerator()

  def mean(x:Vector[Double]) = x.sum / x.size
  def sd(x: Vector[Double]) = {
    val m = mean(x)
    val ss = x.map(xi => math.pow(xi - m,2)).sum
    math.sqrt(ss / x.size)
  }
  def variance(x:Vector[Double]) = math.pow(sd(x), 2)

  def assertApprox(x:Double, y:Double, eps:Double=.01, debug:Boolean=false) = {
    val valid = math.abs(x - y) < eps
    if (debug) {
      if (!valid) {
        println(s"invalid -- x: $x; y:$y")
      } else println("valid")
    }
    assert(valid)
  }

  
  val n = 1E5.toInt
  println

  testWithMsg("Random Uniform") {
    import scala.collection.parallel.immutable.ParVector
    val (xmin, xmax) = (2,20)
    val x = List.fill(n)(runif(2,20))
    assert( x.max < xmax && x.min > xmin)
  }

  testWithMsg("Random Normal") {
    val x = List.fill(n)(rnorm(5,2))
  }

  testWithMsg("Random Exponential") {
    val x = List.fill(n)(rexp(3))
  }

  testWithMsg("Random Gamma") {
    val testShape = (0.1 to 2.0 by 0.1).toList.map(round(_, 1))
    val testRate = (0.1 to 2.0 by 0.1).toList.map(round(_, 1))
    val niter = 1E4.toInt

    for (shape <- testShape; rate <- testRate) {
      val x = Vector.fill(niter){ rgamma(shape, rate) }
      val xMean = mean(x)
      val xVar = variance(x)
      val trueMean = shape / rate
      val trueVar = shape / math.pow(rate,2)
      //println(s"shape: $shape, rate: $rate")
      assertApprox(xMean, trueMean, trueMean/10.0, debug=false)
      assertApprox(xVar, trueVar, trueVar/5.0, debug=false)
    }
  }

  testWithMsg("Random Gamma Speed") {
    val niter = 1E6.toInt
    val x = Vector.fill(niter){ rgamma(shape=.2, rate=.3) }
  }

  testWithMsg("Random Inverse Gamma") {
    val testA = (2.1 to 3.0 by 0.1).toList.map(round(_, 1))
    val testB = (2.1 to 3.0 by 0.1).toList.map(round(_,1))
    val niter = 1E5.toInt

    for (a <- testA; b <- testB) {
      val x = Vector.fill(niter){ rinvgamma(a, b) }
      val xMean = mean(x)
      val trueMean = b / (a - 1)
      assertApprox(xMean, trueMean, trueMean/10.0)
    }
  }

  testWithMsg("Random Beta") {
    val testA = (1.1 to 3.0 by 0.1).toList.map(round(_, 1))
    val testB = (1.1 to 3.0 by 0.1).toList.map(round(_,1))
    val niter = 1E4.toInt

    for (a <- testA; b <- testB) {
      val x = Vector.fill(niter){ rbeta(a, b) }
      val xMean = mean(x)
      val trueMean = a / (a + b)
      assertApprox(xMean, trueMean, trueMean/10.0)
    }
  }

  testWithMsg("Random F") {
  }

  testWithMsg("Random t") {
  }

  testWithMsg("Random Weibull") {
  }

  testWithMsg("Random Chi-sq") {
  }

  println 
}
