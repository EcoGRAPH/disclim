getpar <- function(varname, parname, paramtab, rh = NULL) {
  if(is.null(rh)) {
    output <- paramtab[paramtab$variable == varname, parname]
  } else {
    lookup <- paramtab[paramtab$variable == varname,]
    output <- lookup[match(rh, lookup$rh), parname]
  }
  output
}
