
robustness <- function(nm, gs, cs) {

  gs <- apply(gs, 2, function(x) x/sqrt(sum(x^2)))
  cs <- apply(cs, 2, function(x) x/sqrt(sum(x^2)))
  if (!attr(nm, "hasNA")) {
    rob1 <- apply(cs * nm[[2]] %*% gs, 2, sum)
    rob2 <- apply(gs * nm[[1]] %*% cs, 2, sum)
  } else {
    rob1 <- apply(cs * na.multiply(nm[[2]], gs), 2, sum)
    rob2 <- apply(gs * na.multiply(nm[[1]], cs), 2, sum)
  }

  sqrt(rob1) * sqrt(rob2)
}

isa.filter.robust <- function(data, nm, isares, perms=1, g=NULL, dots=TRUE) {

  rob <- robustness(nm, isares$genes, isares$conditions)
  
  perm <- data
  if (is(data, "ExpressionSet")) {
    perm <- t(exprs(data))
  }
  perm[] <- perm[ sample(length(perm)) ]
  nm2 <- normalize(perm, prenormalize=isares$rundata$prenormalize)

  if (is.null(g)) {
    g <- generate.seeds(count=isares$rundata$N, length=nrow(isares$genes),
                        gs=1/nrow(isares$genes))
  }

  tg <- isares$seeddata$tg
  if (length(tg) != ncol(g) && length(unique(tg)) != 1) {
    warning("Different gene thresholds...")
  }
  tg <- rep(tg, length=ncol(g))

  tc <- isares$seeddata$tc
  if (length(tc) != ncol(g) && length(unique(tc)) != 1) {
    warning("Different condition thresholds...")
  }
  tc <- rep(tc, length=ncol(g))

  permres <- isa(nm2, g=g, tc=tc, tg=tg,
                 down=isares$rundata$down,
                 convergence=isares$rundata$convergence,
                 eps=isares$rundata$eps,
                 oscillation=isares$rundata$oscillation,
                 maxiter=isares$rundata$maxiter,
                 miniter=isares$rundata$miniter,
                 dots=dots,
                 threshold.type=isares$rundata$threshold.type,
                 normalization=isares$rundata$normalization)
  
  rob2 <- robustness(nm2, permres$genes, permres$conditions)
  rob2 <- max(rob2, na.rm=TRUE)

  keep <- rob > rob2

  cat(sep="",
      "Eliminating ", sum(!keep), ", and keeping ", sum(keep), " modules.\n")

  isares$genes <- isares$genes[, keep,drop=FALSE]
  isares$conditions <- isares$conditions[, keep,drop=FALSE]
  isares$seeddata <- isares$seeddata[ keep,,drop=FALSE ]
  isares$rundata$rob.limit <- rob2
  rob <- rob[ keep ]
  isares$seeddata$rob <- rob

  isares
}
