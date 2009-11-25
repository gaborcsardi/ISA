########################################################################
# order EISA ###########################################################
########################################################################

if (require(eisa)) {
	setMethod("OrderEV", signature(biclusters="ISAModules"), 
		function(biclusters, initialorder, maxtime, debuglevel) 
		OrderEV.ISAModules(biclusters, initialorder, maxtime, debuglevel)
	)
}

OrderEV.ISAModules <- function(biclusters, initialorder, maxtime, debuglevel) {

	if ( missing(initialorder) ) {
		initialorder <- NULL	
	} else {
		initialorder <- list(rows=initialorder$genes, cols=initialorder$samples, status=initialorder$status)
	}
	if ( missing(maxtime) ) {
		maxtime <- 60
	}
	if ( missing(debuglevel) ) {
		debuglevel <- 0
	}
	
	isamodules <- eisa:::ISAModules.to.isa.result(biclusters)
	resp <- OrderEV(isamodules, initialorder, maxtime, debuglevel)
	res <- list(genes=resp$rows, samples=resp$cols, status=list(genes=resp$status[[1]], samples=resp$status[[2]]))
	res
}


########################################################################
# order biclust ########################################################
########################################################################

if (require(biclust)) {
	setMethod("OrderEV", signature(biclusters="Biclust"), 
		function(biclusters, initialorder, maxtime, debuglevel) 
		OrderEV.Biclust(biclusters, initialorder, maxtime, debuglevel)
	)
}

OrderEV.Biclust <- function(biclusters, initialorder, maxtime, debuglevel) {
	if ( missing(initialorder) ) {
		initialorder <- NULL	
	}
	if ( missing(maxtime) ) {
		maxtime <- 60
	}
	if ( missing(debuglevel) ) {
		debuglevel <- 0	
	}

	eisamodules <- as(biclusters, "ISAModules")
	OrderEV(eisamodules, initialorder, maxtime, debuglevel)
}


########################################################################
# order ISA ############################################################
########################################################################

if (require(isa2)) {
	setMethod("OrderEV", signature(biclusters="list"), 
		function(biclusters, initialorder, maxtime, debuglevel)
		OrderEV.list(biclusters, initialorder, maxtime, debuglevel)
	)
}

OrderEV.list <- function(biclusters, initialorder, maxtime, debuglevel) {
	
	if ( missing(initialorder) ) {
		initialorder <- NULL	
	}
	if ( missing(maxtime) ) {
		maxtime <- 60
	}
	if ( missing(debuglevel) ) {
		debuglevel <- 0	
	}
		
	if( !is.null(initialorder) && sum(initialorder$status[[1]], initialorder$status[[2]]) == 
		length(initialorder$status[[1]])+length(initialorder$status[[2]]) ) {
		message("initial order is already fully ordered.")
		return(initialorder)
	}
	
	no.bics <- ncol(biclusters$rows)
	no.rows <- nrow(biclusters$rows)
	no.cols <- nrow(biclusters$columns)
	
	no.slots <- c(no.rows, no.cols)
	
	if ( is.null(initialorder) ) {
		initialorder <- list()
		row.map <- list()
		row.map[[1]] <- c(1:no.rows)
		col.map <- list()
		col.map[[1]] <- c(1:no.rows)		
		for ( mod in 1:no.bics ) {
			row.map[[mod + 1]] <- c(1:sum(biclusters[[1]][,mod] != 0))
			col.map[[mod + 1]] <- c(1:sum(biclusters[[2]][,mod] != 0))
		}
		initialorder <- list(rows=row.map, cols=col.map, status=list(vector("numeric",no.bics+1), vector("numeric",no.bics+1)))
	}

	clusters <- list(matrix(as.integer(biclusters[[1]] != 0), nrow=dim(biclusters[[1]])[1]), 
					matrix(as.integer(biclusters[[2]] != 0), nrow=dim(biclusters[[2]])[1]))

	intersections <- list()
	for ( mod in 1:no.bics ) {

		temp <- list()
		for ( i in 1:2 ) {

			temp[[i]] <- vector("integer", 0)
			for ( modp in 1:no.bics) {
				if ( sum(clusters[[i]][,mod]*clusters[[i]][,modp]) > 0 && mod != modp ) {
					temp[[i]] <- append(temp[[i]], modp)
				}
			}
			
		}
			
		intersections[[mod]] <- intersect(temp[[1]], temp[[2]])
	}
	
	# allocate time according to scaling
	# log t = 1.87 log(no.bics) + 2.63 log(no.slots) - 15.20
	estimates <- list()
	estimatedtotal <- 0

	for ( i in 1:2 ) {
		estimates[[i]] <- list( exp(1.87*log(no.bics) + 2.63*log(no.slots[[i]]) - 15.20)*(1-initialorder$status[[i]][[1]]) )
		estimatedtotal <- estimatedtotal + estimates[[i]][[1]]
		for ( mod in 1:no.bics ) {
			estimates[[i]][[mod+1]] <- exp(1.87*log(length(intersections[[mod]])) + 2.63*log(no.slots[[i]]) - 15.20)*(1-initialorder$status[[i]][[mod+1]])
			estimatedtotal <- estimatedtotal + estimates[[i]][[mod+1]]
		}
	}
			
	timelimits <- list()
	for ( i in 1:2 ) {
		timelimits[[i]] <- list(maxtime * estimates[[i]][[1]] / estimatedtotal)
		if ( maxtime > 0 && timelimits[[i]][[1]] < 1 ) {
			timelimits[[i]][[1]] <- 1
		}
		for ( mod in 1:no.bics ) {
			timelimits[[i]][[mod+1]] <- maxtime * estimates[[i]][[mod+1]] / estimatedtotal
			if ( maxtime > 0 && timelimits[[i]][[mod+1]] < 1 ) {
				timelimits[[i]][[mod+1]] <- 1
			}
		}
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
			res <- .Call("orderClusters", clusters[[i]], initialorder[[i]][[1]], as.integer(timelimits[[i]][[1]]),  as.integer(debuglevel))
			map[[1]] <- head(res, -1)
			initialorder$status[[i]][[1]] <- tail(res, 1)
		} else {
			map[[1]] <- initialorder[[i]][[1]]
		}

		# modules
		for ( mod in 1:no.bics ) {
						
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
						for ( modp in 1:no.bics) {
							if ( clusters[[i]][slot, modp] == 1 && modp %in% contains ) {
								subclusters[slotp, which(contains==modp)] <- as.integer(1)
							}
						}
					}
				}
				
				if ( !initialorder$status[[i]][[mod+1]] ) {
					res <- .Call("orderClusters", subclusters, initialorder[[i]][[mod+1]], as.integer(timelimits[[i]][[mod+1]]), as.integer(debuglevel))
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

	res <- list(rows=row.map, cols=col.map, status=list(rows=initialorder$status[[1]], cols=initialorder$status[[2]]))

	return(res)

}
