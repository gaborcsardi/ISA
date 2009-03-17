.onLoad <- function(dir, package) {
  library.dynam("isa", package, dir, local=FALSE);
}

.onUnload <- function(libpath) {
  library.dynam.unload("isa", libpath)
}

.Last.lib <- function(libpath) {
  isa::.onUnload(libpath)
}
