ratesum <- function(tminmax, paramtab) {
  temps <- hourly_sig(tminmax)
  vcs <- vc_life(temps, paramtab)
  mean(vcs)
}
