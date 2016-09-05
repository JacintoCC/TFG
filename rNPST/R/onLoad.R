.onLoad <- function(libname, pkgname){
    library(rJava)
    .jinit("rNPST/java")
}
