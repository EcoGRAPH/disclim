# Simple function to compute hourly temperature based on a sigmoidal surve
hourly_sig <- function(tminmax) {
  hour <- 1:24
  htemp <- (tminmax[[2]] - tminmax[[1]])/2 * sin(2 * pi * hour/24) + (tminmax[[1]] + tminmax[[2]])/2
  htemp
}
