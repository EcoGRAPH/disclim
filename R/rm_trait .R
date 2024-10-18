rm_trait <- function(zj, a, bmax, z) {
  z_rate <- 1/z
  z_rate <- ifelse(z_rate > 1, 1, z_rate)
  zj_rate <- zj
  #a_time <- 1/a
  #a_time <- ifelse(a_time < 0, 0, a_time)
  rm = ((0.01 + z_rate) * ((log(bmax/(0.01 + z_rate))) + log(zj_rate))) /
    (a * (0.01 + z_rate) + 1)
  #rm = ((0.01 + z_rate) * (log(bmax/(0.01 + z_rate)) - a * zj_rate )) /
  #  (a * (0.01 + z_rate) + 1)
  rm
}

