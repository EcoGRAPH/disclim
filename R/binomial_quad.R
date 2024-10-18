binomial_quad <- function(temp, b0, b1, b2) {
  resp <- b0 + b1 * temp + b2 * temp^2
  resp <- exp(resp)/(1 + exp(resp))
  resp
}
