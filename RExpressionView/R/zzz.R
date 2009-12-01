.onLoad <- function(dir, package) {
    library.dynam("ExpressionView", package, dir, local=FALSE);
}

.onUnload <- function(libpath) {
    library.dynam.unload("ExpressionView", libpath)
}

.Last.lib <- function(libpath) {
    isa2::.onUnload(libpath)
}
