.onLoad <- function(libname, pkgname){
    library(rJava)
    library(MCMCpack)
    .jpackage(pkgname, lib.loc = libname)
}
