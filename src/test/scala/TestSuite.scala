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
  import distributions._RandomTest._
  //import distributions.Random._

  import org.apache.commons.math3.special.Gamma.{gamma => gammaFunction}

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

    lazy val ab = {for (a <- testA; b <- testB) yield (a,b)}.toList
    ab.foreach { case (a,b) => 
      val x = Vector.fill(niter){ rbeta(a, b) }
      val xMean = mean(x)
      val trueMean = a / (a + b)
      assertApprox(xMean, trueMean, trueMean/10.0)
    }
  }

  testWithMsg("Random Chi-sq") {
    val testNu = (0.1 to 2.0 by 0.1).toList.map(round(_, 1))
    val niter = 1E4.toInt

    testNu.foreach { nu => 
      val x = Vector.fill(niter){ rchisq(nu) }
      val xMean = mean(x)
      val xVar = variance(x)
      val trueMean = nu
      val trueVar = nu * 2
      assertApprox(xMean, trueMean, trueMean * 0.1)
      assertApprox(xVar, trueVar, trueMean * .25)
    }
  }

  testWithMsg("Random t") {
    val testNu = (3.1 to 4.0 by 0.1).toList.map(round(_, 1))
    val niter = 1E4.toInt

    testNu.foreach { nu => 
      val x = Vector.fill(niter){ rtdist(nu) }
      val xMean = mean(x)
      val xVar = variance(x)
      val trueMean = 0
      val trueVar = nu / (nu - 2)
      if (nu > 2) assertApprox(xMean, trueMean, .1)
      if (nu > 2) assertApprox(xVar, trueVar, trueVar * .5)
    }
  }

  testWithMsg("Random F") {
    val niter = 1E6.toInt
    val (d1, d2) = (3.0, 5.0)
    if (d2 > 4) {
      val x = Vector.fill(niter){ rF(d1, d2) }
      val xMean = mean(x)
      val trueMean = d2 / (d2-2) 
      val xVar = variance(x)
      val trueVar = 2 * d2*d2 * (d1+d2-2) / (d1 * math.pow(d2-2, 2) * (d2-4))
      assertApprox(xMean, trueMean, trueMean * .1, debug=true)
      assertApprox(xVar, trueVar, trueVar * .2, debug=true)
    }
  }


  testWithMsg("Random Weibull") {
    import math.pow
    val niter = 1E6.toInt
    val (shape, scale) = (3.0, 5.0)
    val x = Vector.fill(niter){ rweibull(shape, scale) }
    val xMean = mean(x)
    val trueMean = scale * gammaFunction(1 + 1/shape)
    val xVar = variance(x)
    assertApprox(xMean, trueMean, trueMean * .1, debug=true)
    val trueVar = pow(scale,2) * (gammaFunction(1+2/shape) - pow(gammaFunction(1+1/shape),2))
    assertApprox(xVar, trueVar, trueVar * .2, debug=true)
  }

  println 
}
