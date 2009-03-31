.onLoad <- function(dir, package) {
  library.dynam("isa", package, dir, local=FALSE);

  ## Default ISA options
  isa.options[["verbose"]] <-  FALSE
  isa.options[["status.function"]] <- function(...) isa.status.default(...)
  
}

.onUnload <- function(libpath) {
  library.dynam.unload("isa", libpath)
}

.Last.lib <- function(libpath) {
  isa::.onUnload(libpath)
}
