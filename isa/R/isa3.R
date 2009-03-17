
isa.normalize <- function(data, prenormalize=TRUE, ...) {
  
  require(Biobase)

  ## Create an ExpressionSet first
  if (!is(data, "ExpressionSet")) {
    data <- new("ExpressionSet", exprs=data, ...)
  }

  ## Then normalize it
  ec.exprs <- scale(t(exprs(data)))
  if (prenormalize) {
    eg.exprs <- scale(t(ec.exprs))
  } else {
    eg.exprs <- scale(exprs(data))
  }

  data@assayData <- assayDataNew(exprs=exprs(data),
                                 eg.exprs=eg.exprs,
                                 ec.exprs=t(ec.exprs))
  
  attr(data, "prenormalize") <- prenormalize
  attr(data, "hasNA") <- (any(is.na(data@assayData$eg.exprs)) |
                          any(is.na(data@assayData$ec.exprs)) )
  
  data
}

isa <- function(eset, gene.seeds, cond.seeds,
                thr.gene, thr.cond=thr.gene,
                direction=c("updown", "updown"),
                convergence=c("cor", "loosy", "eps"),
                cor.limit=0.99, eps=1e-4,
                oscillation=TRUE, maxiter=100) {

  if (( missing(gene.seeds) &&  missing(cond.seeds))) {
    stop("No seeds, nothing to do")
  }
  if (!missing(gene.seeds) && nrow(gene.seeds) != nrow(exprs(eset))) {
    stop("Invalid gene seed length")
  }
  if (!missing(cond.seeds) && nrow(cond.seeds) != ncol(exprs(eset))) {
    stop("Invalud conditions seed length")
  }
  
  if (thr.gene < 0 || thr.cond < 0) {
    warning("Negative thresholds, are you sure about this?")
  }
  
  direction <- rep(direction, length=2)
  if (any(!direction %in% c("up", "down", "updown"))) {
    stop("Invalid `direction' argument, should be `down', `up' or `updown'.")
  }

  convergence <- match.arg(convergence)
  if (convergence == "cor") {
    if (cor.limit <= 0) {
      warning("Non-positive correlation limit for convergence.")
    }
  }
  if (convergence == "eps") {
    if (eps >= 1) {
      warning("`eps' limit for convergence greater than one.")
    }
  }
  
  no.seeds <- 0
  if (!missing(gene.seeds)) {
    no.seeds <- no.seeds + ncol(gene.seeds)
  }
  if (!missing(cond.seeds)) {
    no.seeds <- no.seeds + ncol(cond.seeds)
  }
  
  orig.tg <- thr.gene
  orig.tc <- thr.cond
  if (length(thr.gene) != 1 && length(thr.gene) != no.seeds) {
    stop("`thr.gene' does not have the right length")
  }
  if (length(thr.cond) != 1 && length(thr.cond) != no.seeds) {
    stop("`thr.cond' does not have the right length")
  }
  thr.gene <- rep(thr.gene, no.seeds)
  thr.cond <- rep(thr.cond, no.seeds)

  ## Put the seeds together
  all.seeds <- matrix(ncol=0, nrow=nrow(exprs(eset)))
  if (!missing(gene.seeds)) {
    all.seeds <- cbind(all.seeds, gene.seeds)
  }
  if (!missing(cond.seeds)) {
    cond.seeds <- isa.gene.from.cond(eset, cond.seeds=cond.seeds,
                                     thr.gene=tail(thr.gene, ncol(gene.seeds)),
                                     direction=direction[2])
    all.seeds <- cbind(all.seeds, cond.seeds)
  }

  ## All the data about this ISA run
  rundata <- list(direction=direction, eps=eps, cor.limit=cor.limit,
                  maxiter=maxiter, N=no.seeds, convergence=convergence,
                  prenormalize=attr(eset, "prenormalize"),
                  hasNA=attr(eset, "hasNA"),
                  unique=FALSE, oscillation=oscillation,
                  oscillation.fixed=FALSE)

  ## All the seed data, this will be updated, of course
  seeddata <- data.frame(iterations=NA, oscillation=0,
                         thr.gene=thr.gene, thr.cond=thr.cond,
                         freq=rep(1, no.seeds), rob=rep(NA, no.seeds))
  
  if (length(all.seeds)==0) {
    return(list(genes=all.seeds, conditions=matrix(ncol=0, nrow=ncol(eset)),
                rundata=rundata, seeddata=seeddata))
  }

  ## Choose convergence checking function
  if (convergence=="eps") {
    check.convergence <- function(gene.old, gene.new, cond.old, cond.new) {
      res <- (apply(gene.old-gene.new, 2, function(x) all(abs(x)<eps)) &
              apply(cond.old-cond.new, 2, function(x) all(abs(x)<eps)))
      res & !is.na(res)
    }
  } else if (convergence=="cor") {
    check.convergence <- function(gene.old, gene.new, cond.old, cond.new) {
      g.o <- scale(gene.old)
      g.n <- scale(gene.new)
      c.o <- scale(cond.old)
      c.n <- scale(cond.new)
      res <- (colSums(g.o * g.n) / (nrow(g.o)-1) > cor.limit &
              colSums(c.o * c.n) / (nrow(c.o)-1) > cor.limit)
      res & !is.na(res)
    }
  }

  ## Initialize a couple of things
  iter <- 0
  index <- seq_len(ncol(all.seeds))
  if (oscillation) { fire <- character(no.seeds) }
  gene.old <- all.seeds
  cond.old <- matrix(NA, nrow=ncol(eset), ncol=no.seeds)
  gene.res <- matrix(NA, nrow=nrow(eset), ncol=no.seeds)
  cond.res <- matrix(NA, nrow=ncol(eset), ncol=no.seeds)
  
  ## Main loop starts here
  while (TRUE) {

    iter <- iter + 1
    one.step <- isa.step(eset, genes=gene.old, thr.gene=thr.gene,
                         thr.cond=thr.cond, direction=direction)

    gene.new <- one.step[[2]]
    cond.new <- one.step[[1]]

    ## Mark converged seeds
    conv <- check.convergence(gene.old=gene.old, gene.new=gene.new,
                              cond.old=cond.old, cond.new=cond.new)

    ## Mark all zero seeds
    zero <- apply(gene.new, 2, function(x) all(x==0))
    
    ## Mark oscillating ones, if requested
    if (oscillation && iter > 1) {
      new.fire <- apply(gene.new, 2, function(x) sum(round(x, 4)))
      fire <- paste(sep=":", fire, new.fire)
      osc <- logical(ncol(gene.new))
      osc[ (mat <- regexpr("(:.*:.*)\\1$", fire)) != -1] <- TRUE
      osc <- osc & !conv
      
      if (any(osc)) {
        mat <- cbind(mat[osc], attributes(mat, "match.length")[[1]][osc])
        mat <- sapply(seq(length=nrow(mat)), function(x) substr(fire[osc][x],
                            mat[x,1], mat[x,1]+mat[x,2]))
        mat <- sapply(mat, function(x) sum(utf8ToInt(x) == 58), USE.NAMES=FALSE )
        oscresult[index[osc]] <- mat/2
      }
    } else {
      osc <- FALSE
    }
    
    ## These are all to throw out
    drop <- which(conv | zero | osc)
    
    ## Drop the seeds to be dropped
    if (length(drop) != 0) {
      gene.res[,index[drop]] <- gene.new[,drop]
      cond.res[,index[drop]] <- cond.new[,drop]
      seeddata$iterations[index[drop]] <- iter
      gene.new <- gene.new[,-drop,drop=FALSE]
      cond.new <- cond.new[,-drop,drop=FALSE]
      if (oscillation) { fire <- fire[-drop] }
      thr.gene <- thr.gene[-drop]
      thr.cond <- thr.cond[-drop]
      index <- index[-drop]
    }
    
    if (ncol(gene.new)==0 || iter>maxiter) { break; }

    gene.old <- gene.new
    cond.old <- cond.new
    
  } ## End of main loop

  list(genes=gene.res, conditions=cond.res,
       rundata=rundata, seeddata=seeddata)
}

na.multiply <- function(A, B) {
  M <- !is.na(A)
  modA <- A
  modA[!M] <- 0
  v <- modA %*% B
  w <- sqrt(M %*% B^2)
  w2 <- sqrt(apply(B^2, 2, sum))
  ret <- v/w * rep(w2, each=nrow(v))
  ret[ w==0 ] <- 0
  ret
}

isa.step <- function(eset, genes, thr.gene, thr.cond, direction) {

  Ec <- eset@assayData$ec.exprs
  Eg <- t(eset@assayData$eg.exprs)

  direction <- rep(direction, length=2)
  if (any(!direction %in% c("up", "updown", "down"))) {
    stop("Invalid `direction' argument")
  }

  filter <- function(x, t, dir) {
    if (dir=="updown") {
      x <- .Call("beta_filter_updown_vart", x, as.double(t), PACKAGE="isa")
    } else if (dir=="up") {
      x <- .Call("beta_filter_up_vart", x, as.double(t), PACKAGE="isa")
    } else { ## dir=="down"
      x <- .Call("beta_filter_down_vart", x, as.double(t), PACKAGE="isa")
    }
  }

  if ("hasNA" %in% names(attributes(eset)) && !attr(eset, "hasNA")) {
    cond.new <- filter(Eg %*% genes,    thr.cond, direction[1])
    gene.new <- filter(Ec %*% cond.new, thr.gene, direction[2])
  } else {
    cond.new <- filter(na.multiply(Eg, genes   ), thr.cond, direction[1])
    gene.new <- filter(na.multiply(Ec, cond.new), thr.gene, direction[2])
  }

  list(cond.new, gene.new)
}

isa.fix.oscillation <- function(eset, isaresult) {

  if (!isaresult$rundata$oscillation) {
    stop("No oscillating modules were searched")
  }

  if (all(isaresult$seeddata$oscillation == 0) ) {
    return(isaresult)
  }

  rerun <- which(isaresult$seeddata$oscillation != 0)
  len <- isaresult$seeddata$oscillation[rerun]

  for (s in seq_along(rerun)) {
    res <- list(matrix(0, nr=nrow(isaresult$genes), nc=len[s]),
                matrix(0, nr=nrow(isaresult$conditions), nc=len[s]))
    res[[1]][,1] <- isaresult$genes[,rerun[s]]
    res[[2]][,1] <- isaresult$conditions[,rerun[s]]
    for (i in 1:(len[s]-1)) {
      tmp <- isa.step(eset, res[[1]][,i,drop=FALSE],
                      thr.gene=isaresult$seeddata$thr.gene[rerun[s]],
                      thr.cond=isaresult$seeddata$the.cond[rerun[s]],
                      direction=isaresult$rundata$direction)
      res[[1]][,i+1] <- tmp[[2]]
      res[[2]][,i+1] <- tmp[[1]]
    }
    chosen <- which.min(apply(res[[1]], 2, sum))
    isaresult$genes[,rerun[s]] <- res[[1]][,chosen]
    isaresult$conditions[,rerun[s]] <- res[[2]][,chosen]
  }

  isaresult
}

isa.unique <- function(eset, isaresult, method=c("cor", "round"),
                       ignore.div=TRUE, cor.limit=0.99, neg.cor=TRUE,
                       drop.zero=TRUE) {

  method <- match.arg(method)

  if (ncol(isaresult$genes) == 0) { return(isaresult) }

  ## drop divergent seeds
  if (ignore.div) {
    invalid <- is.na(isaresult$seeddata$iterations)
    if (any(invalid)) {
      valid <- !invalid
      isaresult$genes <- isaresult$genes[,valid,drop=FALSE]
      isaresult$conditions <- isaresult$conditions[,valid,drop=FALSE]
      isaresult$seeddata <- isaresult$seeddata[valid,,drop=FALSE]
    }
  }
  if (ncol(isaresult$genes) == 0) { return(isaresult) }
  
  ## drop all zero seeds
  if (drop.zero) {
    valid <- apply(isaresult$genes, 2, function(x) any(x != 0))
    if (!all(valid)) {
      isaresult$genes <- isaresult$genes[,valid,drop=FALSE]
      isaresult$conditions <- isaresult$conditions[,valid,drop=FALSE]
      isaresult$seeddata <- isaresult$seeddata[valid,,drop=FALSE]
    }
  }
  if (ncol(isaresult$genes) == 0) { return(isaresult) }

  if (method=="cor") {
    if (neg.cor) { ABS <- abs } else { ABS <- function(x) x }
    cm <- pmin(ABS(cor(isaresult$genes)), ABS(cor(isaresult$conditions)))
    cm[ lower.tri(cm, diag=TRUE) ] <- 0
    uni <- apply(cm < cor.limit, 2, all)
    freq <- apply(cm >= cor.limit, 1, sum)[uni] + 1
  } else if (method=="round") {
    ## TODO
  }

  isaresult$genes <- isaresult$genes[,uni,drop=FALSE]
  isaresult$conditions <- isaresult$conditions[,uni,drop=FALSE]

  isaresult$seeddata <- isaresult$seeddata[uni,,drop=FALSE]
  isaresult$seeddata$freq <- freq

  isaresult$rundata$unique <- TRUE

  isaresult      
}


isa.gene.from.cond <- function(eset, cond.seeds, thr.cond, direction) {


  Ec <- eset@assayData$ec.exprs

  if (! direction %in% c("up", "updown", "down")) {
    stop("Invalid `direction' argument")
  }

  filter <- function(x, t, dir) {
    if (dir=="updown") {
      x <- .Call("beta_filter_updown_vart", x, as.double(t), PACKAGE="isa")
    } else if (dir=="up") {
      x <- .Call("beta_filter_up_vart", x, as.double(t), PACKAGE="isa")
    } else { ## dir=="down"
      x <- .Call("beta_filter_down_vart", x, as.double(t), PACKAGE="isa")
    }
  }

  if ("hasNA" %in% names(attributes(eset)) && !attr(eset, "hasNA")) {
    gene.new <- filter(Ec %*% cond.seeds, thr.gene, direction)
  } else {
    gene.new <- filter(na.multiply(Ec, cond.seeds), thr.gene, direction)
  }

  gene.new
}  

generate.seeds <- function(length, count=100, method=c("uni"),
                           gs, ...) {

  if (method == "uni") {
    if (missing(gs)) {
      gs <- sample(1:length, count, replace=TRUE)
    } else {
      gs <- rep(round(gs*length), length=count)
    }
    g <- matrix(0, nrow=length, ncol=count)
    for (i in 1:count) {
      g[sample(length, gs[i]),i] <- 1
    }
  }
  g
}
