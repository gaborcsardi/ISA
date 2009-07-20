setMethod("orderModules", signature(modules="list"),
	function(modules, ...) orderModules.default(modules, ...))

orderModules.default <- function(modules, debuglevel=0, timelimit=60) {
	#isa.status("ISA ordering", "in")
	
	no.mods <- ncol(modules$rows)
	no.rows <- nrow(modules$rows)
	no.cols <- nrow(modules$columns)
		
	for ( i in 1:2 ) {

		if ( i == 1 ) {
			no.slots <- no.rows
		} else {
			no.slots <- no.cols
		}
				
		matrix <- modules[[i]]
		
		clusters <- mat.or.vec(no.slots, no.mods)
		for ( mod in 1:no.mods ) {
			for ( slot in 1:no.slots ) {
				if ( matrix[slot, mod] != 0. ) {
					clusters[slot, mod] <- 1
				}
			}
		}
		
		map <- list()

		# global
		if ( i == 1 ) {
			cat("ordering", no.rows, "genes\r")
		} else {
			cat("ordering", no.cols, "samples\r")
		}
		flush.console()
		
		map[[1]] <- .Call("order", clusters, debuglevel, timelimit)

		# modules
		for ( mod in 1:no.mods ) {
			
			if ( i == 1 ) {
				cat("ordering genes in module", mod, "\r")
			} else {
				cat("ordering samples in module", mod, "\r")
			}
			flush.console()
			
			nslots <- sum(clusters[,mod])
		
			contains <- mat.or.vec(0,1)
			for ( slot in 1:no.slots ) {
				if ( clusters[slot, mod] == 1 ) {
					for ( modp in 1:no.mods) {
						if ( clusters[slot, modp] == 1 && modp != mod ) {
							contains <- append(contains, modp)
						}
					}
				}
			}
			contains <- sort(unique(contains))
			
			if ( length(contains) > 1 ) {

				invcontains <- mat.or.vec(length(contains),1)
				for ( modp in 1:length(contains) ) {
					invcontains[contains[[modp]]] <- modp
				}

				subclusters <- mat.or.vec(nslots, length(contains))
				slotp <- 0
				for ( slot in 1:no.slots ) {
					if ( clusters[slot, mod] == 1 ) {
						slotp <- slotp + 1
						for ( modp in 1:no.mods) {
							if ( clusters[slot, modp] == 1 && modp != mod ) {
								subclusters[slotp, invcontains[modp]] <- 1
							}
						}
					}
				}
				map[[mod+1]] <- .Call("order", subclusters, debuglevel, timelimit)

			} else {
				if ( length(contains) > 0 ) {
					map[[mod+1]] <- unique(c(contains[[1]],1:nslots))
				} else {
					map[[mod+1]] <- c(1:nslots)
				}
			}
		
		}
	
		if ( i == 1 ) {
			row.map <- map
		} else {
			col.map <- map
		}
	
	}

	cat("ordering done.\r")
	flush.console()

	res = list(rows=row.map, cols=col.map)

	#isa.status("DONE", "out")

	res

}

