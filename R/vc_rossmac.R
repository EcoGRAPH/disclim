vc_rossmac <- function(temp, M, a, lf, PDR, b, c, bc, bcvar = TRUE) {
  mu <- 1/lf
  if(bcvar) {
    VC = M * a^2 * bc * exp(-1 * mu/PDR)/mu
  } else {
    VC = M * a^2 * b * c * exp(-1 * mu/PDR)/mu
  }
  VC
}

