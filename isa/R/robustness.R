
robustness <- function(normed.data, gs, cs) {

  Ec <- normed.data$Ec
  Eg <- normed.data$Eg
  
  gs <- apply(gs, 2, function(x) x/sqrt(sum(x^2)))
  cs <- apply(cs, 2, function(x) x/sqrt(sum(x^2)))
  if ("hasNA" %in% names(attributes(normed.data)) && !attr(normed.data, "hasNA")) {
    rob1 <- colSums(cs * Eg %*% gs)
    rob2 <- colSums(gs * Ec %*% cs)
  } else {
    rob1 <- colSums(cs * na.multiply(Eg, gs))
    rob2 <- colSums(gs * na.multiply(Ec, cs))
  }

  rob1[ rob1 < 0 ] <- 0
  rob2[ rob2 < 0 ] <- 0
  sqrt(rob1) * sqrt(rob2)
}

isa.filter.robust <- function(data, normed.data, isares, perms=1,
                              row.seeds, col.seeds) {

  if (perms <= 0) {
    stop("Number of permutations must be non-negative")
  }

  if (length(unique(isares$seeddata$thr.row)) != 1) {
    warning("Different row thresholds, using only the first one.")
  }
  
  if (length(unique(isares$seeddata$thr.col)) != 1) {
    warning("Different column thresholds, using only the first one.")
  }
  
  isares$seeddata$rob <- robustness(normed.data, isares$rows, isares$columns)

  if (missing(row.seeds) && missing(col.seeds)) {
    row.seeds <- generate.seeds(count=isares$rundata$N,
                                 length=nrow(isares$rows),
                                 gs=2/nrow(isares$rows))
  }

  rob.max <- 0
  
  for (i in seq_len(perms)) {
    data.scrambled <- data
    data.scrambled[] <- sample(data.scrambled)
    normed.data.scrambled <- isa.normalize(data.scrambled)
    
    permres <- isa.iterate(normed.data.scrambled, row.seeds=row.seeds,
                           thr.row=isares$seeddata$thr.row[1],
                           thr.col=isares$seeddata$thr.col[1],
                           direction=isares$rundata$direction,
                           convergence=isares$rundata$convergence,
                           cor.limit=isares$rundata$cor.limit,
                           eps=isares$rundata$eps,
                           oscillation=isares$rundata$oscillation,
                           maxiter=isares$rundata$maxiter)

    valid <- apply(permres$rows != 0, 2, any)
    valid <- valid & apply(permres$columns !=0, 2, any)

    permres$rows <- permres$rows[,valid,drop=FALSE]
    permres$columns <- permres$columns[,valid,drop=FALSE]
    permres$seeddata <- permres$seeddata[valid,,drop=FALSE]

    rob2 <- robustness(normed.data.scrambled, permres$rows, permres$columns)
    if (any(is.na(rob2))) { browser() }
    rob.max <- max(rob2, rob.max, na.rm=TRUE)
  }

  keep <- isares$seeddata$rob > rob.max

  isares$rows <- isares$rows[, keep,drop=FALSE]
  isares$columns <- isares$columns[, keep,drop=FALSE]
  isares$seeddata <- isares$seeddata[ keep,,drop=FALSE ]
  if (nrow(isares$seeddata)>0) { isares$seeddata$rob.limit <- rob.max }
  isares$rundata$rob.perms <- perms

  isares
}
