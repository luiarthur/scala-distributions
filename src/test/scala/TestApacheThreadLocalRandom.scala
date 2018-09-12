import org.scalatest.FunSuite
import distribution.helper._
import org.apache.commons.math3.random.{RandomDataGenerator, RandomGeneratorFactory}
import org.apache.commons.math3.distribution._


class TestApacheThreadLocalRandom extends TestUtil {
  val rng = RandomGeneratorFactory.createRandomGenerator(java.util.concurrent.ThreadLocalRandom.current())
  //val rng = RandomGeneratorFactory.createRandomGenerator(new java.util.Random(0))
  val rdg = new RandomDataGenerator(rng)


  val idx = (0 until 1E6.toInt)

  timer { rdg.nextPoisson(150) }
  timer { idx.foreach{_ => rdg.nextPoisson(150)} } // 5 seconds

  val pois = timer { new PoissonDistribution(rng, 150.0, 1E-12, 10000) }
  timer { idx.foreach{_ => pois.sample } } // 5 seconds
  timer { idx.foreach{_ => { new PoissonDistribution(rng, 150.0, 1E-12, 10000) }.sample } } // 5.6 seconds


  def createRNG() = RandomGeneratorFactory.createRandomGenerator(java.util.concurrent.ThreadLocalRandom.current())
  print("Parallel ThreadLocalRandom: ")
  timer { (0 until 100).par.foreach( _ => {new RandomDataGenerator(createRNG)}.nextPoisson(150) ) }
}
 
