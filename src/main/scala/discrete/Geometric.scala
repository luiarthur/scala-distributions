package distribution.discrete

// TODO: Test
case class Geometric(params: Double) extends NegativeBinomialBase((1, params))
