
isa.option <- function(...) {
  args <- list(...)
  nam <- names(args)

  env <- get(".isa.options", envir=.GlobalEnv)
  if (length(args) == 0) {
    return(as.list(env))
  }
  
  if (is.null(nam)) {
    ## query
    if (length(args) != 1) {
      warning("Can't query many options at once, ignoring the rest")
    }
    if (!is.character(args[[1]]) || length(args[[1]]) != 1) {
      stop("Expected character of length one")
    }
    return(get(args[[1]], env))
  } else {
    ## set, everything must be named
    if (any(nam=="")) {
      stop("Some options are not named")
    }
    for (i in seq_along(nam)) {
      assign(nam[i], args[[i]], envir=env)
    }
    assign(".isa.options", env, envir=.GlobalEnv)
    return(invisible(env))
  }
}
