package distributions

trait RandomGenerator {
  def nextDouble(): Double
  def nextGaussian(): Double = {
    // box-mueller
    math.sqrt(-2 * math.log(nextDouble)) * math.cos(2 * math.Pi * nextDouble)
  }
  def nextInt(numDistinct:Int): Int
  def setSeed(seed:Long): Unit
}

class _ScalaUtilRandom extends RandomGenerator {
  val R = scala.util.Random
  //override def nextGaussian = R.nextGaussian
  def nextDouble = R.nextDouble
  def setSeed(seed:Long) = R.setSeed(seed)
  def nextInt(numDistinct:Int) = R.nextInt(numDistinct)
}

class _ThreadLocalRandom extends RandomGenerator {
  import java.util.concurrent.{ ThreadLocalRandom => R }
  //override def nextGaussian = R.current().nextGaussian
  def nextDouble = R.current.nextDouble
  def nextInt(numDistinct:Int) = R.current.nextInt(numDistinct)
  def setSeed(seed:Long) = R.current.setSeed(seed)
}


