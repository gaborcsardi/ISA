.onLoad <- function(lib, pkg) {
}

.onUnload <- function(libpath) {
}

.Last.lib <- function(libpath) {
  eisa::.onUnload(libpath)
}
