package distributions

trait RandomGenerator {
  def nextGaussian(): Double
  def nextDouble(): Double
  def setSeed(x:Long): Unit
}

class _ScalaUtilRandom extends RandomGenerator {
  val R = scala.util.Random
  override def nextGaussian = R.nextGaussian
  override def nextDouble = R.nextDouble
  override def setSeed(x:Long) = R.setSeed(x)
}

class _ThreadLocalRandom extends RandomGenerator {
  import java.util.concurrent.{ ThreadLocalRandom => R }
  override def nextGaussian = R.current().nextGaussian
  override def nextDouble = R.current.nextDouble
  override def setSeed(x:Long) = R.current.setSeed(x)
}


