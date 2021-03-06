
.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Written by Joo, Seokhoon.")
  options(scipen = 14)
  options(raum.eps = 1e-8)
  options(raum.triple.colors = c("#E41A1C", "#377EB8", "#4DAF4A"))
  options(raum.gender.colors = c("#00BFC4", "#F8766D"))
  if (Sys.info()[["sysname"]] == "Linux")
    Sys.setlocale("LC_MESSAGES", "en_US.UTF-8")
}

.onUnload <- function(libpath) {
  library.dynam.unload("raum", libpath)
}
