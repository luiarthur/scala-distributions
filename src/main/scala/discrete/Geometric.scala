package distribution.discrete

// TODO: Test
// FIXME
case class Geometric(params: Double) extends NegativeBinomialBase((1, params))
