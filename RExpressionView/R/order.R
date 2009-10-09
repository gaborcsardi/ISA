########################################################################
# order EISA ###########################################################
########################################################################

if (require(eisa)) {
	setMethod("OrderEV", signature(modules="ISAModules"), function(modules, ...) OrderEV.ISAModules(modules, ...))
}

OrderEV.ISAModules <- function(modules, initialorder=NULL, debuglevel=0, maxtime=60) {
	isamodules <- eisa:::ISAModules.to.isa.result(modules)
	if ( !is.null(initialorder) ) {
		initialorder <- list(rows=initialorder$genes, cols=initialorder$samples, status=initialorder$status)
	}
	resp <- OrderEV(isamodules, initialorder, debuglevel, maxtime)
	res <- list(genes=resp$rows, samples=resp$cols, status=list(genes=resp$status[[1]], samples=resp$status[[2]]))
	res
}


########################################################################
# order biclust ########################################################
########################################################################

if (require(biclust)) {
	setMethod("OrderEV", signature(modules="Biclust"), function(modules, ...) OrderEV.Biclust(modules, ...))
}

OrderEV.Biclust <- function(modules, initialorder=NULL, debuglevel=0, maxtime=60) {
	eisamodules <- as(modules, "ISAModules")
	OrderEV(eisamodules, initialorder, debuglevel, maxtime)
}


########################################################################
# order ISA ############################################################
########################################################################

if (require(isa2)) {
	setMethod("OrderEV", signature(modules="list"), function(modules, ...) OrderEV.list(modules, ...))
}

OrderEV.list <- function(modules, initialorder=NULL, debuglevel=0, maxtime=60) {	
	no.mods <- ncol(modules$rows)
	no.rows <- nrow(modules$rows)
	no.cols <- nrow(modules$columns)
		
	no.slots <- c(no.rows, no.cols)
	
	if ( is.null(initialorder) ) {
		initialorder <- list()
		row.map <- list()
		row.map[[1]] <- c(1:no.rows)
		col.map <- list()
		col.map[[1]] <- c(1:no.rows)		
		for ( mod in 1:no.mods ) {
			row.map[[mod + 1]] <- c(1:sum(modules[[1]][,mod] != 0))
			col.map[[mod + 1]] <- c(1:sum(modules[[2]][,mod] != 0))
		}
		initialorder <- list(rows=row.map, cols=col.map, status=list(vector("numeric",no.mods+1), vector("numeric",no.mods+1)))
	}

	clusters <- list(matrix(as.integer(modules[[1]] != 0), nrow=dim(modules[[1]])[1]), 
					matrix(as.integer(modules[[2]] != 0), nrow=dim(modules[[2]])[1]))

	intersections <- list()
	for ( mod in 1:no.mods ) {

		temp <- list()
		for ( i in 1:2 ) {

			temp[[i]] <- vector("integer", 0)
			for ( modp in mod:no.mods) {
				if ( sum(clusters[[i]][,mod]*clusters[[i]][,modp]) > 0 && mod != modp ) {
					temp[[i]] <- append(temp[[i]], modp)
				}
			}
			
		}
			
		intersections[[mod]] <- intersect(temp[[1]], temp[[2]])
	}
	
	# allocate time according to scaling
	# log t = 1.87 log(no.mods) + 2.63 log(no.slots) - 15.20
	estimates <- list()
	estimatedtotal <- 0

	for ( i in 1:2 ) {
		estimates[[i]] <- list( exp(1.87*log(no.mods) + 2.63*log(no.slots[[i]]) - 15.20)*(1-initialorder$status[[i]][[1]]) )
		estimatedtotal <- estimatedtotal + estimates[[i]][[1]]
		for ( mod in 1:no.mods ) {
			estimates[[i]][[mod+1]] <- exp(1.87*log(length(intersections[[mod]])) + 2.63*log(no.slots[[i]]) - 15.20)*(1-initialorder$status[[i]][[mod+1]])
			estimatedtotal <- estimatedtotal + estimates[[i]][[mod+1]]
		}
	}
			
	timelimits <- list()
	#cat("timelimits:\n")
	for ( i in 1:2 ) {
		timelimits[[i]] <- list(maxtime * estimates[[i]][[1]] / estimatedtotal)
		if ( maxtime > 0 && timelimits[[i]][[1]] < 1 ) {
			timelimits[[i]][[1]] <- 1
		}
		#cat(timelimits[[i]][[1]], " ")
		for ( mod in 1:no.mods ) {
			timelimits[[i]][[mod+1]] <- maxtime * estimates[[i]][[mod+1]] / estimatedtotal
			if ( maxtime > 0 && timelimits[[i]][[mod+1]] < 1 ) {
				timelimits[[i]][[mod+1]] <- 1
			}
		#	cat(timelimits[[i]][[mod+1]], " ")
		}
		#cat("\n")
	}
	
	for ( i in 1:2 ) {
		
		map <- list()

		# global
		if ( i == 1 ) {
			cat("ordering", no.rows, "rows\r")			} else {
			cat("ordering", no.cols, "columns\r")
		}
		flush.console()
		
		if ( !initialorder$status[[i]][[1]] ) {
			res <- .Call("orderClusters", clusters[[i]], initialorder[[i]][[1]], as.integer(debuglevel), as.integer(timelimits[[i]][[1]]))
			map[[1]] <- head(res, -1)
			initialorder$status[[i]][[1]] <- tail(res, 1)
		} else {
			map[[1]] <- initialorder[[i]][[1]]
		}

		# modules
		for ( mod in 1:no.mods ) {
						
			if ( i == 1 ) {
				cat("ordering rows in module", mod, "\r")
			} else {
				cat("ordering columns in module", mod, "\r")
			}
			flush.console()
			
			nslots <- sum(clusters[[i]][,mod])
					
			contains <- intersections[[mod]]
						
			if ( length(contains) > 0 ) {

				subclusters <- matrix(as.integer(0), nslots, length(contains))
				slotp <- 0
				for ( slot in 1:no.slots[i] ) {
					if ( clusters[[i]][slot, mod] == 1 ) {
						slotp <- slotp + 1
						for ( modp in 1:no.mods) {
							if ( clusters[[i]][slot, modp] == 1 && modp %in% contains ) {
								subclusters[slotp, which(contains==modp)] <- as.integer(1)
							}
						}
					}
				}
				
				if ( !initialorder$status[[i]][[mod+1]] ) {
					res <- .Call("orderClusters", subclusters, initialorder[[i]][[mod+1]], as.integer(debuglevel), as.integer(timelimits[[i]][[mod+1]]))
					map[[mod+1]] <- head(res,-1)
					initialorder$status[[i]][[mod+1]] <- tail(res, 1)
				} else {
					map[[mod+1]] <- initialorder[[i]][[mod+1]]
				}

			} else {
				map[[mod+1]] <- c(1:nslots)
				initialorder$status[[i]][[mod+1]] <- as.integer(1)
			}
		
		}
	
		if ( i == 1 ) {
			row.map <- map
		} else {
			col.map <- map
		}
	
	}

	cat("ordering done.                                  \r\n")
	flush.console()

	res = list(rows=row.map, cols=col.map, status=list(rows=initialorder$status[[1]], cols=initialorder$status[[2]]))

	res

}
