
robustness <- function(eset, gs, cs) {

  Ec <- eset@assayData$ec.exprs
  Eg <- t(eset@assayData$eg.exprs)
  
  gs <- apply(gs, 2, function(x) x/sqrt(sum(x^2)))
  cs <- apply(cs, 2, function(x) x/sqrt(sum(x^2)))
  if ("hasNA" %in% names(attributes(eset)) && !attr(eset, "hasNA")) {
    rob1 <- apply(cs * na.multiply(Eg, gs), 2, sum)
    rob2 <- apply(gs * na.multiply(Ec, cs), 2, sum)
  } else {
    rob1 <- apply(cs * Eg %*% gs, 2, sum)
    rob2 <- apply(gs * Ec %*% cs, 2, sum)
  }

  sqrt(rob1) * sqrt(rob2)
}

isa.filter.robust <- function(eset, isares, perms=1,
                              gene.seeds, cond.seeds) {

  if (!is(eset, "ExpressionSet")) {
    stop("Not an ExpressionSet")
  }

  if (perms <= 0) {
    stop("Number of permutations must be non-negative")
  }

  if (length(unique(isares$seeddata$thr.gene)) != 1) {
    warning("Different gene thresholds, using only the first one.")
  }
  
  if (length(unique(isares$seeddata$thr.cond)) != 1) {
    warning("Different condition thresholds, using only the first one.")
  }
  
  isares$seeddata$rob <- robustness(eset, isares$genes, isares$conditions)

  if (missing(gene.seeds) && missing(cond.seeds)) {
    gene.seeds <- generate.seeds(count=isares$rundata$N,
                                 length=nrow(isares$genes),
                                 gs=2/nrow(isares$genes))
  }

  rob.max <- 0
  
  for (i in perms) {
    eset.scrambled <- eset
    exprs(eset.scrambled)[] <- sample(exprs(eset.scrambled))
    eset.scrambled <- isa.normalize(eset.scrambled)
    
    permres <- isa(eset.scrambled, gene.seeds=gene.seeds,
                   thr.gene=isares$seeddata$thr.gene[1],
                   thr.cond=isares$seeddata$thr.cond[1],
                   direction=isares$rundata$direction,
                   convergence=isares$rundata$convergence,
                   cor.limit=isares$rundata$cor.limit,
                   eps=isares$rundata$eps,
                   oscillation=isares$rundata$oscillation,
                   maxiter=isares$rundata$maxiter)
    
    rob2 <- robustness(eset.scrambled, permres$genes, permres$conditions)
    rob.max <- max(rob2, rob.max)
  }

  keep <- isares$seeddata$rob > rob.max

  isares$genes <- isares$genes[, keep,drop=FALSE]
  isares$conditions <- isares$conditions[, keep,drop=FALSE]
  isares$seeddata <- isares$seeddata[ keep,,drop=FALSE ]
  isares$rundata$rob.limit <- rob.max

  isares
}
