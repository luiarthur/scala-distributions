import org.scalatest.FunSuite
import distribution.helper._
import org.apache.commons.math3.random.{RandomDataGenerator, RandomGeneratorFactory, RandomGenerator}
import org.apache.commons.math3.distribution._


class TestApacheThreadLocalRandom extends TestUtil {
  //val rng = RandomGeneratorFactory.createRandomGenerator(java.util.concurrent.ThreadLocalRandom.current())
  val rng = RandomGeneratorFactory.createRandomGenerator(new java.util.Random(0))
  val rdg = new RandomDataGenerator(rng)


  val idx = (0 until 1E6.toInt)

  timer { rdg.nextPoisson(150) }
  timer { idx.foreach{_ => rdg.nextPoisson(150)} } // 5 seconds

  val pois = timer { new PoissonDistribution(rng, 150.0, 1E-12, 10000) }
  timer { idx.foreach{_ => pois.sample } } // 5 seconds
  timer { idx.foreach{_ => { new PoissonDistribution(rng, 150.0, 1E-12, 10000) }.sample } } // 5.6 seconds


  def createRNG() = {
    RandomGeneratorFactory.createRandomGenerator(java.util.concurrent.ThreadLocalRandom.current())
    //RandomGeneratorFactory.createRandomGenerator(new java.util.Random(0))
  }
  print("Parallel ThreadLocalRandom: ")
  timer { (0 until 100).par.foreach( _ => {new RandomDataGenerator(createRNG)}.nextPoisson(150) ) }
  /*
  {new RandomDataGenerator(createRNG)}.nextPoisson(150) 
  */



  // InverseGamma
  case class InverseGamma(val shape:Double, val scale:Double, rng:RandomGenerator=null) extends AbstractRealDistribution(rng) {
    import math.{log, pow, exp}
    import org.apache.commons.math3.special.Gamma.{logGamma, regularizedGammaQ}
    override def logDensity(x:Double) = if (x > 0) {
      shape * log(scale) - logGamma(shape) - (shape+1) * log(x) - scale / x
    } else {
      Double.NegativeInfinity
    }

    def density(x:Double) = exp(logDensity(x))

    def cumulativeProbability(x:Double) = if (x > 0) {
      regularizedGammaQ(shape, scale / x)
    } else 0

    lazy val mean:Double = if (shape > 1) scale / (shape - 1) else Double.PositiveInfinity
    lazy val variance:Double = if (shape > 2) pow(scale / (shape - 1), 2) / (shape - 2) else Double.PositiveInfinity
    lazy val getNumericalMean: Double = mean
    lazy val getNumericalVariance: Double = variance
    lazy val mode = scale / (shape + 1)
    lazy val getSupportLowerBound: Double = 0.0
    lazy val getSupportUpperBound: Double = Double.PositiveInfinity
    lazy val isSupportConnected:Boolean = true
    lazy val isSupportLowerBoundInclusive:Boolean = false
    lazy val isSupportUpperBoundInclusive:Boolean = false
    override def sample():Double = {
      // Note that Gamma use (shape, scale) parameterization
      1 / (new GammaDistribution(rng, shape, 1 / scale).sample)
    }
  }

  class RandomDataGenerator2(rng:RandomGenerator) extends RandomDataGenerator(rng) {
    def nextInverseGamma(shape:Double, scale:Double):Double = {
      //1 / nextGamma(shape, 1/scale)
      InverseGamma(shape, scale, getRandomGenerator()).sample();
    }
  }

  //val rng = RandomGeneratorFactory.createRandomGenerator(new java.util.Random(0))
  timer{ {new RandomDataGenerator2(rng)}.nextInverseGamma(2,3) }
}
 
