.onLoad <- function(libname, pkgname) {
    library.dynam("ExpressionView", pkgname, libname, local=FALSE);
}

.onUnload <- function(libpath) {
    library.dynam.unload("ExpressionView", libpath)
}

.Last.lib <- function(libpath) {
    isa2::.onUnload(libpath)
}
