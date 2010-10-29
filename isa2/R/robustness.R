
setMethod("robustness", signature(normed.data="list"),
          function(normed.data, ...) robustness.default(normed.data, ...))

robustness.default <- function(normed.data, ...) {
  if (length(normed.data)==2) {
    robustness.isa(normed.data, ...)
  } else if (length(normed.data)==4) {
    robustness.ppa(normed.data, ...)
  }
}

robustness.isa <- function(normed.data, row.scores, col.scores) {

  isa.status("Calculating ISA robustness", "in")

  if (ncol(row.scores) != ncol(col.scores)) {
    stop("Row and column column dimensions don't match")
  }
  
  if (ncol(row.scores)==0) return(numeric())
  
  Ec <- normed.data$Ec
  Er <- normed.data$Er
  
  row.scores <- apply(row.scores, 2, function(x) x/sqrt(sum(x^2)))
  col.scores <- apply(col.scores, 2, function(x) x/sqrt(sum(x^2)))
  if ("hasNA" %in% names(attributes(normed.data)) && !attr(normed.data, "hasNA")) {
    rob1 <- colSums(col.scores * Er %*% row.scores)
    rob2 <- colSums(row.scores * Ec %*% col.scores)
  } else {
    rob1 <- colSums(col.scores * na.multiply(Er, row.scores))
    rob2 <- colSums(row.scores * na.multiply(Ec, col.scores))
  }

  rob1[ rob1 < 0 ] <- 0
  rob2[ rob2 < 0 ] <- 0
  res <- sqrt(rob1) * sqrt(rob2)

  isa.status("DONE", "out")

  res
}

robustness.ppa <- function(normed.data, row1.scores, row2.scores,
                           col.scores) {

  isa.status("Calculating PPA robustness", "in")

  if (!is.matrix(row1.scores) || !is.matrix(row2.scores) ||
      !is.matrix(col.scores)) {
    stop("`row1.scores', `row2.scores' and `col.scores' must be matrices")
  }
  
  if (ncol(row1.scores) != ncol(row2.scores) ||
      ncol(row1.scores) != ncol(col.scores)) {
    stop("`row1.scores', `row2.scores' and `col.scores' must have the same",
         "number of columns")
  }

  if (ncol(row1.scores)==0) return (numeric())

  row1.scores <- apply(row1.scores, 2, function(x) x/sqrt(sum(x^2)))
  row2.scores <- apply(row2.scores, 2, function(x) x/sqrt(sum(x^2)))
  col.scores  <- apply(col.scores,  2, function(x) x/sqrt(sum(x^2)))

  hasNA <- c(TRUE, TRUE)
  if ("hasNA" %in% names(attributes(normed.data))) {
    hasNA <- attr(normed.data, "hasNA")
  }

  if (!hasNA[1]) {
    rob1 <- colSums( col.scores * normed.data$Egc %*% row1.scores)
    rob4 <- colSums(row1.scores * normed.data$Ecg %*%  col.scores)
  } else {
    rob1 <- colSums( col.scores * na.multiply(normed.data$Egc, row1.scores))
    rob4 <- colSums(row1.scores * na.multiply(normed.data$Ecg,  col.scores))
  }

  if (!hasNA[2]) {
    rob2 <- colSums(row2.scores * normed.data$Ecd %*%  col.scores)
    rob3 <- colSums( col.scores * normed.data$Edc %*% row2.scores)
  } else {
    rob2 <- colSums(row2.scores * na.multiply(normed.data$Ecd,  col.scores))
    rob3 <- colSums( col.scores * na.multiply(normed.data$Edc, row2.scores))
  }

  rob1[ rob1 < 0 ] <- 0
  rob2[ rob2 < 0 ] <- 0
  rob3[ rob3 < 0 ] <- 0
  rob4[ rob4 < 0 ] <- 0
  
  res <- sqrt(rob1) * sqrt(rob2) * sqrt(rob3) * sqrt(rob4)

  isa.status("DONE", "out")

  res
}

setMethod("isa.filter.robust", signature(data="matrix"),
          function(data, ...) isa.filter.robust.default(data, ...))

isa.filter.robust.default <- function(data, normed.data, isares, perms=1,
                                      row.seeds, col.seeds) {
  
  isa.status("Filtering for robustness...", "in")
  
  if (perms <= 0) {
    stop("Number of permutations must be non-negative")
  }

  if (length(isares$rows) == 0) {
    return(isares)
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
                                 length=nrow(isares$rows))
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
    rob.max <- max(rob2, rob.max, na.rm=TRUE)
  }

  keep <- isares$seeddata$rob > rob.max

  isares$rows <- isares$rows[, keep,drop=FALSE]
  isares$columns <- isares$columns[, keep,drop=FALSE]
  isares$seeddata <- isares$seeddata[ keep,,drop=FALSE ]
  if (nrow(isares$seeddata)>0) { isares$seeddata$rob.limit <- rob.max }
  isares$rundata$rob.perms <- perms

  isa.status("DONE.", "out")
  
  isares
}
