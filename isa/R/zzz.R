.onLoad <- function(dir, package) {
  library.dynam("isa", package, dir, local=FALSE);
  if (!exists(".isa.options", envir=.GlobalEnv)) {
    env <- new.env()
    
    assign("verbose", FALSE, envir=env)
    assign("status.function", function(...) { isa.status.default(...) },
           envir=env)
    
    assign(".isa.options", env, envir=.GlobalEnv)    
  }
}

.onUnload <- function(libpath) {
  library.dynam.unload("isa", libpath)
}

.Last.lib <- function(libpath) {
  isa::.onUnload(libpath)
}
