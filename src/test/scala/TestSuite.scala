import org.scalatest.FunSuite
import distribution.helper.timer
import org.apache.commons.math3.random.RandomDataGenerator

class TestSuite extends TestUtil {
  // For reproducibility, don't use ThreadLocalRandom for testing.
  //import distribution.RandomPar._

  // Use scala.util.Rnadom instead
  object R extends distribution.RandomSeq(new scala.util.Random(10))
  import R._

  //val commonsMathR = new RandomDataGenerator()

  val n = 1E5.toInt
  println

  testWithMsg("choleskyL") {
    import distribution.SpecialFunctions._
    val mat = Array(Array(25.0, 15.0, -5.0), 
                    Array(15.0, 18.0,  0.0),
                    Array(-5.0,  0.0, 11.0))
    val trueChoL = Array(Array( 5.0, 0.0, 0.0), 
                         Array( 3.0, 3.0, 0.0),
                         Array(-1.0, 1.0, 3.0))
    val cholMat = choleskyL(mat)
    printMat(cholMat)
    assertApproxMat(cholMat, trueChoL)

    val case2 = Map("x" ->
      Array(Array(18.0,  22.0,   54.0,   42.0), 
            Array(22.0,  70.0,   86.0,   62.0), 
            Array(54.0,  86.0,  174.0,  134.0), 
            Array(42.0,  62.0,  134.0,  106.0)),
      "truth" -> 
      Array(Array( 4.24264, 0.00000, 0.00000, 0.00000),
            Array( 5.18545, 6.56591, 0.00000, 0.00000),
            Array(12.72792, 3.04604, 1.64974, 0.00000),
            Array( 9.89949, 1.62455, 1.84971, 1.39262))
    )
    val chol2 = choleskyL(case2("x"))
    printMat(chol2)
    assertApproxMat(chol2, case2("truth"))

    val dim = 300
    val eyeMat = eye(dim)
    print(s"Time taken to compute cholesky of ${dim}x${dim}: ")
    timer {
      choleskyL(eyeMat)
    }
  }

  testWithMsg("vvMult") {
    import distribution.SpecialFunctions._
    val a = Array(1.0, 2.0 ,3.0)
    val b = Array(2.0, 3.0 ,4.0)
    val c = vvMult(a, b)
    assert( c == 20.0 )
  }

  testWithMsg("isSquare") {
    import distribution.SpecialFunctions._
    val squareMat = Array.ofDim[Double](5,5)
    assert( isSquare(squareMat) )

    val nonsquareMat = Array.ofDim[Double](5,3)
    assert( !isSquare(nonsquareMat) )
  }



  testWithMsg("Random Uniform") {
    //import scala.collection.parallel.immutable.ParVector
    val (xmin, xmax) = (2,20)
    val x = List.fill(n)(nextUniform(2,20))
    assert( x.max < xmax && x.min > xmin)
  }

  testWithMsg("Random Normal") {
    val (m,s) = (5,2)
    val x = Vector.fill(1E6.toInt)(nextGaussian(m,s))
    val xMean = mean(x)
    val xSd = sd(x)
    assertApprox(xMean, m, m * .01)
    assertApprox(xSd, s, s * .1)
  }

  testWithMsg("Random Exponential") {
    val lam = 3.0
    val x = Vector.fill(1E6.toInt)(nextExponential(lam))
    val xMean = mean(x)
    val xVar = variance(x)
    val trueMean = 1 / lam
    val trueVar = 1 / (lam*lam)
    assertApprox(xMean, trueMean, trueMean * .01)
    assertApprox(xVar, trueVar, trueVar * .1)
  }

  testWithMsg("Random Gamma") {
    //val testShape = (0.1 to 2.0 by 0.1).toList.map(round(_, 1))
    val testShape = List(0.1, 1.0, 2.0)
    val testRate = List(0.1, 1.0, 2.0)
    val niter = 5E5.toInt

    for (shape <- testShape; rate <- testRate) {
      val x = Vector.fill(niter){ nextGamma(shape, rate) }
      val xMean = mean(x)
      val xVar = variance(x)
      val trueMean = distribution.continuous.Gamma(shape, rate).mean
      val trueVar = distribution.continuous.Gamma(shape, rate).variance
      assertApprox(xMean, trueMean, trueMean * .01, debug=false)
      assertApprox(xVar, trueVar, trueVar*.1, debug=false)
    }
  }

  testWithMsg("Random Gamma Speed") {
    val niter = 1E6.toInt
    val x = Vector.fill(niter){ nextGamma(shape=.2, rate=.3) }
  }

  testWithMsg("Random Inverse Gamma") {
    val testA = List(3.5)
    val testB = List(2.5)
    val niter = 1E7.toInt

    for (a <- testA; b <- testB) {
      val x = Vector.fill(niter){ nextInverseGamma(a, b) }
      val xMean = mean(x)
      val xVar = variance(x)
      val trueMean = distribution.continuous.InverseGamma(a, b).mean
      val trueVar = distribution.continuous.InverseGamma(a, b).variance
      assertApprox(xMean, trueMean, trueMean * .1, debug=true)
      assertApprox(xVar, trueVar, trueVar * .1, debug=true)
    }
  }

  testWithMsg("Random Beta") {
    //val testA = (1.1 to 3.0 by 0.1).toList.map(round(_, 1))
    //val testB = (1.1 to 3.0 by 0.1).toList.map(round(_,1))
    val testA = List(1.1, 2.0, 3.0).map(round(_, 1))
    val testB = List(1.1, 2.0, 3.0).map(round(_,1))
    val niter = 1E4.toInt

    lazy val ab = {for (a <- testA; b <- testB) yield (a,b)}.toList
    ab.foreach { case (a,b) => 
      val x = Vector.fill(niter){ nextBeta(a, b) }
      val xMean = mean(x)
      val trueMean = a / (a + b)
      assertApprox(xMean, trueMean, trueMean/10.0)
    }
  }

  testWithMsg("Random Chi-sq") {
    val testNu = (0.1 to 2.0 by 0.1).toList.map(round(_, 1))
    val niter = 1E4.toInt

    testNu.foreach { nu => 
      val x = Vector.fill(niter){ nextChisq(nu) }
      val xMean = mean(x)
      val xVar = variance(x)
      val trueMean = nu
      val trueVar = nu * 2
      assertApprox(xMean, trueMean, trueMean * 0.1, debug=false)
      assertApprox(xVar, trueVar, trueMean * .35, debug=false)
    }
  }

  testWithMsg("Random t") {
    val testNu = (3.1 to 4.0 by 0.1).toList.map(round(_, 1))
    val niter = 1E4.toInt

    testNu.foreach { nu => 
      val x = Vector.fill(niter){ nextT(nu) }
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
    val (d1, d2) = (3.0, 10.0)
    require(d2 > 4.0)
    val x = Vector.fill(niter){ nextF(d1, d2) }
    val xMean = mean(x)
    val trueMean = d2 / (d2-2) 
    val xVar = variance(x)
    val trueVar = 2 * d2*d2 * (d1+d2-2) / (d1 * math.pow(d2-2, 2) * (d2-4))
    assertApprox(xMean, trueMean, trueMean * .1, debug=true)
    assertApprox(xVar, trueVar, trueVar * .1, debug=true)
  }


  testWithMsg("Random Weibull") {
    import org.apache.commons.math3.special.Gamma.{gamma => gammaFunction}
    import math.pow
    val niter = 1E6.toInt
    val (shape, scale) = (3.0, 5.0)
    val x = Vector.fill(niter){ nextWeibull(shape, scale) }
    val xMean = mean(x)
    val trueMean = scale * gammaFunction(1 + 1/shape)
    val xVar = variance(x)
    assertApprox(xMean, trueMean, trueMean * .1)
    val trueVar = pow(scale,2) * (gammaFunction(1+2/shape) - pow(gammaFunction(1+1/shape),2))
    assertApprox(xVar, trueVar, trueVar * .2)
  }

  testWithMsg("Random Logistic") {
    val niter = 1E6.toInt
    val (m, s) = (3.0, 2.0)
    val x = Vector.fill(niter){ nextLogistic(m, s) }
    val xMean = mean(x)
    val trueMean = distribution.continuous.Logistic(m, s).mean
    val xVar = variance(x)
    val trueVar = distribution.continuous.Logistic(m, s).variance
    assertApprox(xMean, trueMean, trueMean * .1)
    assertApprox(xVar, trueVar, trueVar * .2)
  }
 
  testWithMsg("Random Poisson") {
    val niter = 1E6.toInt
    val testLam = List(3,40)
    testLam.foreach{lam => 
      val x = Vector.fill(niter)(nextPoisson(lam).toDouble)
      val xMean = mean(x)
      val xVar = variance(x)
      val trueMean, trueVar = lam
      assertApprox(xMean, trueMean, trueMean * .1)
      assertApprox(xVar, trueVar, trueVar * .1)
    }
  }

  testWithMsg("Random Negative Binomial") {
    import distribution.discrete.NegativeBinomial

    def numFailuresTillSuccesses(n:Int, p:Double, numSuccesses:Int=0, numFailures:Int=0): Int = {
      if (numSuccesses == n) numFailures else {
        val success = nextBernoulli(p)
        numFailuresTillSuccesses(n, p, numSuccesses+success, numFailures+(1-success))
      }
    }
    val testN = 5
    val testP = .3
    val niter = 1E6.toInt
    val x = Vector.fill(niter)(nextNegativeBinomial(testN, testP).toDouble)
    val xMean = mean(x)
    val xVar = variance(x)
    val y = Vector.fill(niter)(numFailuresTillSuccesses(testN, testP).toDouble)
    val simMean = mean(y)
    val simVar = variance(y)
    //val trueMean = testN * (1-testP) / testP
    val trueMean = NegativeBinomial(testN, testP).mean
    val trueVar = NegativeBinomial(testN, testP).variance
    assertApprox(xMean, simMean, simMean * .1, debug=false)
    assertApprox(xVar, simVar, simVar * .1, debug=false)
    assertApprox(xMean, trueMean, trueMean * .1, debug=false)
    assertApprox(xVar, trueVar, trueMean * .1, debug=false)
  }

  testWithMsg("Random wsampleIndex") {
    val niter=1E6.toInt
    val probs = Vector(.2, .3, .5)
    //val x = Vector.fill(niter){ wsampleIndex(probs).toDouble }
    val x = Vector.fill(niter){ wsampleIndex(probs.map(_ *100)).toDouble }
    val trueMean = probs.zipWithIndex.map{ case (i, p) => i * p }.sum
    val trueVar = probs.zipWithIndex.map{ case (i, p) => i * i * p }.sum
    assertApprox(mean(x), trueMean, trueMean * .01, debug=true)
    assertApprox(variance(x), trueVar, trueVar * .05, debug=true)
  }

  testWithMsg("Random Dirichlet") {
    // TODO: Other tests
    val a = Vector(2000.0, 1000.0, 1000.0)
    val x = nextDirichlet(a)
    assertApprox(x.sum, 1, 1E-5)
    a.indices.foreach{ i =>
      assertApprox(x(i), a(i)/a.sum, .02, debug=true)
    }
  }

  testWithMsg("nextInt") {
    val x = nextInt(10)
  }

  testWithMsg("Random MVNormal") {
    import distribution.SpecialFunctions._
    val dim = 30
    val m = Array.range(0, dim).map(_.toDouble)
    val covMat = eye(dim); covMat(1)(1) = 0.5
    val n = 2E5.toInt
    val xs = Array.fill(n)(nextMvNormal(m, covMat))
    val xsMean = xs.transpose.map{ x => mean(x.toVector) }
    val xsVar = xs.transpose.map{ x => variance(x.toVector) }
    val trueVarDiag = covMat.indices.map(i => covMat(i)(i))
    xsMean.zip(m.toVector).foreach{ case (a,b) => assertApprox(a,b,1E-2) }
    xsVar.zip(trueVarDiag.toVector).foreach{ case (a,b) => assertApprox(a,b,1E-2) }
  }

  testWithMsg("Random nextMultinomial") {
    val m = 1E6.toInt
    val prob = Array(2.0, 3.0, 4.0)
    val draw = nextMultinomial(m, prob)
    val drawNormalized = draw.map{ _.toDouble / m }
    val trueProp = prob.map{ p => p / prob.sum }
    assert(draw.sum == m)
    assertApproxArray(drawNormalized, trueProp, 1E-2, debug=true)
  }

  /*
  testWithMsg("Apache Speed Test") {
    import org.apache.commons.math3.random.{ MersenneTwister => Rng }
    object AR extends distribution.RandomApache(new Rng(10))
    val AF = new org.apache.commons.math3.random.RandomDataGenerator

    val n = 1E6.toInt
    val idx = (0 until n)

    timer{ print("Beta mine:   "); idx.foreach{ _ => nextBeta(3, 5) } }
    timer{ print("Beta apache fast: "); idx.foreach{ _ => AF.nextBeta(3, 5) } }
    timer{ print("Beta apache: "); idx.foreach{ _ => AR.nextBeta(3, 5) } }
    println

    timer{ print("Gamma mine:   "); idx.foreach{ _ => nextGamma(3, 5) } }
    timer{ print("Gamma apache fast: "); idx.foreach{ _ => AF.nextGamma(3, 5) } }
    timer{ print("Gamma apache: "); idx.foreach{ _ => AR.nextGamma(3, 5) } }
    println

    import distribution.SpecialFunctions.eye
    val m = Array.fill(10)(0.0)
    val S = eye(10)
    timer{ print("MVN mine:   "); idx.foreach{ _ => nextMvNormal(m, S) } }
    //timer{ print("MVN apache fast: "); idx.foreach{ _ => AF.nextMvNormal(m, S) } }
    timer{ print("MVN apache: "); idx.foreach{ _ => AR.nextMvNormal(m, S) } }
    println
  }
  */

  // TODO
  testWithMsg("new Distribution vs direct random Speed") {
    import distribution.continuous.Normal
    R.setSeed(10)
    val rng = new distribution.RandomSeq(new scala.util.Random(10))

    assert(nextGaussian() == (new Normal()).sample(rng))

    val n = 1E6.toInt
    val idx = (0 until n)
    println
    timer{ print("Distribution:   "); idx.foreach{ _ => (new Normal(2, 3)).sample(rng) } }
    timer{ print("Random:         "); idx.foreach{ _ => nextGaussian(2, 3) } }
    println
  }
 
  println 
}
