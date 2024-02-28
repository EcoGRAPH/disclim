calcsun <- function(doy, latitude) {
  Gamma <- 2 * pi/365 * ((doy) - 1)
  Delta <- 180/pi * (0.006918 - 0.399912 * cos(Gamma) + 0.070257 *
                       sin(Gamma) - 0.006758 * cos(Gamma) + 0.000907 * sin(Gamma) -
                       0.002697 * cos(3 * (Gamma)) + 0.00148 * sin(3 * (Gamma)))
  CosWo <- (sin(-0.8333/360 * 2 * pi) - sin(latitude/360 * 2 * pi) *
              sin(Delta/360 * 2 * pi))/(cos(latitude/360 *
                                              2 * pi) * cos(Delta/360 * 2 * pi))
  Sunrise <- 12 - acos(CosWo)/(15/360 * 2 * pi)
  Sunset <- 12 + acos(CosWo)/(15/360 * 2 * pi)
  Daylength <- Sunset - Sunrise
  results <- c(Sunrise, Sunset, Daylength)
  results
}
