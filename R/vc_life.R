vc_life <- function(M, a, G, bc, b, c, bcvar = TRUE) {
  if(bcvar) {
    VC = a^2 * bc * G * M
  } else {
    VC = a^2 * b * c * G * M
  }
VC
}

