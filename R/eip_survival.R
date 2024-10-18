eip_survival <- function(temp, lf, PDR) {
  mu <- 1/lf
  P = exp(-1 * mu/PDR)
  P
}

