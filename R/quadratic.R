quadratic <- function(temp, cc, t0, tm) {
  resp <- -cc * (temp - t0) * (temp - tm)
  resp[temp >= tm] <- 0
  resp[temp <= t0] <- 0
  resp
}
