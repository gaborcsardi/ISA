if (require(eisa)) {
  setMethod("orderModules", signature(modules="ISAModules"),
            function(modules, ...) orderModules.ISAModules(modules, ...))
}

orderModules.ISAModules <- function(modules, initialorder=NULL, debuglevel=0, maxtime=60) {
	isamodules <- eisa:::ISAModules.to.isa.result(modules)
	if ( !is.null(initialorder) ) {
		initialorder <- list(rows=initialorder$genes, cols=initialorder$rows, initialorder$status)
	}
	resp <- orderModules(isamodules, initialorder, debuglevel, maxtime)
	res <- list(genes=resp$rows, samples=resp$cols, status=list(genes=resp$status[[1]], samples=resp$status[[2]]))
	res
}
