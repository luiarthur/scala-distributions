package distribution

class RandomSeq(rng: scala.util.Random) extends RandomGeneric {
  def setSeed(seed:Long):Unit = rng.setSeed(seed)
  def nextBoolean(): Boolean = rng.nextBoolean()
  def nextInt(n:Int):Int = rng.nextInt(n)
  def nextLong(): Long = rng.nextLong()
  def nextFloat(): Float = rng.nextFloat()
  def nextDouble():Double = rng.nextDouble()
}

object RandomPar extends RandomGeneric {
  import java.util.concurrent.{ ThreadLocalRandom => rng }
  def setSeed(seed:Long):Unit = rng.current().setSeed(seed)
  def nextBoolean(): Boolean = rng.current().nextBoolean()
  def nextInt(n:Int):Int = rng.current().nextInt(n)
  def nextLong(): Long = rng.current().nextLong()
  def nextFloat(): Float = rng.current().nextFloat()
  def nextDouble():Double = rng.current().nextDouble()
}

// Think about this...
//class RandomApache(rng: org.apache.commons.math3.random.RandomGenerator) extends RandomGeneric {
//  import org.apache.commons.math3.distribution.MultivariateNormalDistribution
//  import org.apache.commons.math3.distribution.BetaDistribution
//
//  def setSeed(seed:Long):Unit = rng.setSeed(seed)
//  def nextBoolean(): Boolean = nextBoolean()
//  def nextInt(n:Int):Int = rng.nextInt()
//  def nextLong(): Long = rng.nextLong()
//  def nextFloat(): Float = nextFloat()
//  def nextDouble():Double = rng.nextDouble()
//
//  override def nextBeta(a:Double, b:Double):Double = {
//    { new BetaDistribution(a, b) }.sample
//  }
//
//  override def nextMvNormal(m:Array[Double], cov:Array[Array[Double]]): Array[Double] = {
//    { new MultivariateNormalDistribution(rng, m, cov) }.sample
//  }
//}
