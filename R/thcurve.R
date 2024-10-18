thcurve <- function(temp, rh, variable, paramtab, rhvalues = NULL) {

  if(is.null(rhvalues)) {
    rh2 <- rh
  } else {
    #rh2 <- which.min(abs(rhvalues - rh))
    rh2 <- rhvalues[max.col(-abs(outer(rh,rhvalues,"-")))]
  }

  eqtype = paramtab[paramtab$variable == variable, "form"][1]

  if(eqtype == "quadratic") {
    cc = getpar(variable, "p1", paramtab, rh = rh2)
    tmin = getpar(variable, "p2", paramtab, rh = rh2)
    tmax = getpar(variable, "p3", paramtab, rh = rh2)
    traits <- quadratic(temp, cc, tmin, tmax)
  } else if(eqtype == "briere") {
    cc = getpar(variable, "p1", paramtab, rh = rh2)
    tmin = getpar(variable, "p2", paramtab, rh = rh2)
    tmax = getpar(variable, "p3", paramtab, rh = rh2)
    traits <- briere(temp, cc, tmin, tmax)
  } else if (eqtype == "exponential") {
    aa = getpar(variable, "p1", paramtab, rh = rh2)
    cc = getpar(variable, "p2", paramtab, rh = rh2)
    r = getpar(variable, "p3", paramtab, rh = rh2)
    traits <- exponential(temp, aa, cc, r)
  } else if (eqtype == "binomial_quad") {
    b0 = getpar(variable, "p1", paramtab, rh = rh2)
    b1 = getpar(variable, "p2", paramtab, rh = rh2)
    b2 = getpar(variable, "p3", paramtab, rh = rh2)
    traits <- binomial_quad(temp, b0, b1, b2)
  }
  traits

}
