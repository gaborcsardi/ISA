if (require(eisa)) {
  setMethod("orderModules", signature(modules="ISAModules"),
            function(modules, ...) orderModules.ISAModules(modules, ...))
}

orderModules.ISAModules <- function(modules, debuglevel=0, timelimit=60) {
	isamodules <- ISAModules.to.isa.result(modules)
	resp <- isa2::orderModules(isamodules, debuglevel, timelimit)
	res <- list(genes=resp$rows, samples=resp$cols)
	res
}
