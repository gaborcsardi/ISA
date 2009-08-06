if (require(isa2)) {
  setMethod("orderModules", signature(modules="list"),
            function(modules, ...) orderModules.default(modules, ...))
}

orderModules.default <- function(modules, debuglevel=0, timelimit=60) {
	#isa.status("ISA ordering", "in")
	
	no.mods <- ncol(modules$rows)
	no.rows <- nrow(modules$rows)
	no.cols <- nrow(modules$columns)
	
	no.slots <- c(no.rows, no.cols)
	clusters = list(mat.or.vec(no.rows, no.mods), mat.or.vec(no.cols, no.mods))
	intersections = list()
	
	# determine intersecting modules
	for ( mod in 1:no.mods ) {
	
		intersection <- list()
		for ( i in 1:2 ) {

			contains <- mat.or.vec(0, 1)
			for ( slot in 1:no.slots[i] ) {
				if ( modules[[i]][slot, mod] != 0 ) {
					clusters[[i]][slot, mod] <- 1
					
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
	

	
	for ( i in 1:2 ) {
						
		map <- list()

		# global
		if ( i == 1 ) {
			cat("ordering", no.rows, "genes\r")			} else {
			cat("ordering", no.cols, "samples\r")
		}
		flush.console()
		
		map[[1]] <- .Call("order", clusters[[i]], debuglevel, timelimit)

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

				subclusters <- matrix(0, nslots, length(contains))
				slotp <- 0
				for ( slot in 1:no.slots[i] ) {
					if ( clusters[[i]][slot, mod] == 1 ) {
						slotp <- slotp + 1
						for ( modp in 1:no.mods) {
							if ( clusters[[i]][slot, modp] == 1 && modp %in% contains ) {
								subclusters[slotp, which(contains==modp)] <- 1
							}
						}
					}
				}
				
				map[[mod+1]] <- .Call("order", subclusters, debuglevel, timelimit)

			} else {
				map[[mod+1]] <- c(1:nslots)
			}
		
		}
	
		if ( i == 1 ) {
			row.map <- map
		} else {
			col.map <- map
		}
	
	}

	cat("ordering done.                                  \r")
	flush.console()

	res = list(rows=row.map, cols=col.map)

	#isa.status("DONE", "out")

	res

}
