.onLoad <- function(dir, package) {
}

.onUnload <- function(libpath) {
}

.Last.lib <- function(libpath) {
  eisa::.onUnload(libpath)
}
