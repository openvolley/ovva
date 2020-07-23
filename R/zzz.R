.onAttach <- function(libname, pkgname) {
    shiny::addResourcePath("ovvajs", system.file("extdata", "js", package = "ovva"))
}
