getpar <- function(varname, parname, paramtab) {
  output <- paramtab[paramtab$variable == varname, parname]
  output
}
