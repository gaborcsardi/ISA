.First.lib <- function(lib, pkg) {
  library.dynam("isa", pkg, lib)
}

.Last.lib <- function(libpath) {
  library.dynam.unload("isa", libpath)
}

