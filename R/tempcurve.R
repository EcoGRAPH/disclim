tempcurve <- function(temp, variable, paramtab) {
  eqtype = paramtab[paramtab$variable == variable, "form"]
  rc = getpar(variable, "rc", paramtab)
  tmin = getpar(variable, "tmin", paramtab)
  tmax = getpar(variable, "tmax", paramtab)
  if(eqtype == "quadratic") {
    traits <- quadratic(temp, rc, tmin, tmax)
  } else if(eqtype == "briere") {
    traits <- briere(temp, rc, tmin, tmax)
  }
  traits
}
