.onLoad <- function(libname, pkgname){
    library(rJava)
    .jpackage(pkgname, lib.loc = libname)
}
