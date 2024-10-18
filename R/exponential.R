exponential <- function(temp, aa, cc, r) {
  resp <- aa * exp(-r * temp) + cc
  resp[resp < 0] <- 0
  resp
}
