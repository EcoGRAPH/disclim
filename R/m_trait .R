m_trait <- function(temp, lf, EFD, pEA, MDR) {
  mu <- 1/lf
  M = EFD * pEA * MDR / (mu)^2
  M
}

