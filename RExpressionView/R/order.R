########################################################################
# order EISA ###########################################################
########################################################################

if (require(eisa)) {
	setMethod("order.clusters", signature(modules="ISAModules"), function(modules, ...) order.clusters.ISAModules(modules, ...))
}

order.clusters.ISAModules <- function(modules, initialorder=NULL, debuglevel=0, maxtime=60) {
	isamodules <- eisa:::ISAModules.to.isa.result(modules)
	if ( !is.null(initialorder) ) {
		initialorder <- list(rows=initialorder$genes, cols=initialorder$samples, status=initialorder$status)
	}
	resp <- order.clusters(isamodules, initialorder, debuglevel, maxtime)
	res <- list(genes=resp$rows, samples=resp$cols, status=list(genes=resp$status[[1]], samples=resp$status[[2]]))
	res
}


########################################################################
# order biclust ########################################################
########################################################################

if (require(biclust)) {
	setMethod("order.clusters", signature(modules="Biclust"), function(modules, ...) order.clusters.Biclust(modules, ...))
}

order.clusters.Biclust <- function(modules, initialorder=NULL, debuglevel=0, maxtime=60) {
	eisamodules <- as(modules, "ISAModules")
	order.clusters(eisamodules, initialorder, debuglevel, maxtime)
}


########################################################################
# order ISA ############################################################
########################################################################

if (require(isa2)) {
	setMethod("order.clusters", signature(modules="list"), function(modules, ...) order.clusters.default(modules, ...))
}

order.clusters.default <- function(modules, initialorder, debuglevel, maxtime) {
	
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
		initialorder <- list(rows=row.map, cols=col.map, status=list(vector("numeric",no.mods+1),vector("numeric",no.mods+1)))
	}
	
	clusters = list(matrix(as.integer(0), no.rows, no.mods), matrix(as.integer(0), no.cols, no.mods))
	intersections = list()
	
	# determine intersecting modules
	for ( mod in 1:no.mods ) {
	
		intersection <- list()
		for ( i in 1:2 ) {

			contains <- mat.or.vec(0, 1)
			for ( slot in 1:no.slots[i] ) {
				if ( modules[[i]][slot, mod] != 0 ) {
					clusters[[i]][slot, mod] <- as.integer(1)
					
					for ( modp in 1:no.mods ) {
						if ( modp != mod ) {
							if ( modules[[i]][slot, modp] != 0 ) {
								contains <- append(contains, modp)
							}
						}
					}
					
				}
			}
			
			intersection[[i]] <- sort(unique(contains))

		}
		
		intersections[[mod]] <- intersect(intersection[[1]], intersection[[2]])

	}
	
	# allocate time according to scaling
	timelimits <- list()
	for ( i in 1:2 ) {
		timelimits[[i]] <- list(maxtime / 4)
		for ( mod in 1:no.mods ) {
			timelimits[[i]][[mod+1]] <- maxtime / 4 / no.mods * length(intersections[[mod]])
		}
	}
	
	for ( i in 1:2 ) {
		
		map <- list()

		# global
		if ( i == 1 ) {
			cat("ordering", no.rows, "genes\r")			} else {
			cat("ordering", no.cols, "samples\r")
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
				cat("ordering genes in module", mod, "\r")
			} else {
				cat("ordering samples in module", mod, "\r")
			}
			flush.console()
			
			nslots <- sum(clusters[[i]][,mod])
					
			contains <- intersections[[mod]]
						
			if ( length(contains) >= 1 ) {

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
