#' @title Automatic execution function on load of package
#'
#' @description This function executes when package is loades
#' @param libname Name of the library
#' @param pkgname Name of the package
.onLoad <- function(libname, pkgname){
    # Load of necessary libraries
    suppressMessages(suppressWarnings(require(rJava)))
    suppressMessages(suppressWarnings(require(MCMCpack)))
    suppressMessages(suppressWarnings(require(ggtern)))
    suppressMessages(suppressWarnings(require(XML)))
    # Init of the rJava in the package location
    .jpackage(pkgname, lib.loc = libname)
}
