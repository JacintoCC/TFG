.onLoad <- function(libname, pkgname){
    library(rJava)
    .jinit("rNPTS/java")
}
