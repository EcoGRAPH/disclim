#Briere, J.F., Pracros, P., le Roux, A.Y. and Pierre, S. (1999) A novel rate model of #temperature-dependent development for arthropods. Environmental Entomology, 28, 22-29.

briere <- function(temp, cc, t0, tm) {
  resp <- cc * temp * (temp - t0) * (tm - temp)^(1/2)
  resp[temp >= tm] <- 0
  resp[temp <= t0] <- 0
  resp
}