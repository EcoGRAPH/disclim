tempcurve <- function(temp, variable, paramtab) {
  eqtype = paramtab[paramtab$variable == variable, "form"]
  if(eqtype == "quadratic") {
    cc = getpar(variable, "p1", paramtab)
    tmin = getpar(variable, "p2", paramtab)
    tmax = getpar(variable, "p3", paramtab)
    traits <- quadratic(temp, cc, tmin, tmax)
  } else if(eqtype == "briere") {
    cc = getpar(variable, "p1", paramtab)
    tmin = getpar(variable, "p2", paramtab)
    tmax = getpar(variable, "p3", paramtab)
    traits <- briere(temp, cc, tmin, tmax)
  } else if (eqtype == "exponential") {
    aa = getpar(variable, "p1", paramtab)
    cc = getpar(variable, "p2", paramtab)
    r = getpar(variable, "p3", paramtab)
    traits <- exponential(temp, aa, cc, r)
  } else if (eqtype == "binomial_quad") {
    b0 = getpar(variable, "p1", paramtab)
    b1 = getpar(variable, "p2", paramtab)
    b2 = getpar(variable, "p3", paramtab)
    traits <- binomial_quad(temp, b0, b1, b2)
  }
  traits
}
