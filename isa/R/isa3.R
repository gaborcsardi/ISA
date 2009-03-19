
isa.normalize <- function(data, prenormalize=FALSE) {

  if (!is.matrix(data)) {
    stop("`data' must be a matrix")
  }
  
  ## Then normalize it
  Ec <- scale(t(data))
  if (prenormalize) {
    Eg <- scale(t(Ec))
  } else {
    Eg <- scale(data)
  }

  data <- list(Eg=t(Eg), Ec=t(Ec))
  
  attr(data, "prenormalize") <- prenormalize
  attr(data, "hasNA") <- (any(is.na(Eg)) |
                          any(is.na(Ec)) )
  
  data
}

isa <- function(normed.data, row.seeds, col.seeds,
                thr.row, thr.col=thr.row,
                direction=c("updown", "updown"),
                convergence=c("cor", "loosy", "eps"),
                cor.limit=0.99, eps=1e-4,
                oscillation=TRUE, maxiter=100) {

  if (( missing(row.seeds) &&  missing(col.seeds))) {
    stop("No seeds, nothing to do")
  }
  if (!missing(row.seeds) && nrow(row.seeds) != ncol(normed.data$Eg)) {
    stop("Invalid row seed length")
  }
  if (!missing(col.seeds) && nrow(col.seeds) != ncol(normed.data$Ec)) {
    stop("Invalid column seed length")
  }
  
  if (thr.row < 0 || thr.col < 0) {
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
  if (!missing(row.seeds)) {
    no.seeds <- no.seeds + ncol(row.seeds)
  }
  if (!missing(col.seeds)) {
    no.seeds <- no.seeds + ncol(col.seeds)
  }
  
  orig.tg <- thr.row
  orig.tc <- thr.col
  if (length(thr.row) != 1 && length(thr.row) != no.seeds) {
    stop("`thr.row' does not have the right length")
  }
  if (length(thr.col) != 1 && length(thr.col) != no.seeds) {
    stop("`thr.col' does not have the right length")
  }
  thr.row <- rep(thr.row, no.seeds)
  thr.col <- rep(thr.col, no.seeds)

  ## Put the seeds together
  all.seeds <- matrix(ncol=0, nrow=nrow(normed.data$Ec))
  if (!missing(row.seeds)) {
    all.seeds <- cbind(all.seeds, row.seeds)
  }
  if (!missing(col.seeds)) {
    col.seeds <- isa.row.from.col(normed.data, col.seeds=col.seeds,
                                  thr.row=tail(thr.row, ncol(row.seeds)),
                                  direction=direction[2])
    all.seeds <- cbind(all.seeds, col.seeds)
  }

  ## All the data about this ISA run
  rundata <- list(direction=direction, eps=eps, cor.limit=cor.limit,
                  maxiter=maxiter, N=no.seeds, convergence=convergence,
                  prenormalize=attr(normed.data, "prenormalize"),
                  hasNA=attr(normed.data, "hasNA"),
                  unique=FALSE, oscillation=oscillation,
                  oscillation.fixed=FALSE)

  ## All the seed data, this will be updated, of course
  seeddata <- data.frame(iterations=NA, oscillation=0,
                         thr.row=thr.row, thr.col=thr.col,
                         freq=rep(1, no.seeds), rob=rep(NA, no.seeds))
  
  if (length(all.seeds)==0) {
    return(list(rows=all.seeds, columns=matrix(ncol=0, nrow=ncol(normed.data$Ec)),
                rundata=rundata, seeddata=seeddata))
  }

  ## Choose convergence checking function
  if (convergence=="eps") {
    check.convergence <- function(row.old, row.new, col.old, col.new) {
      res <- (apply(row.old-row.new, 2, function(x) all(abs(x)<eps)) &
              apply(col.old-col.new, 2, function(x) all(abs(x)<eps)))
      res & !is.na(res)
    }
  } else if (convergence=="cor") {
    check.convergence <- function(row.old, row.new, col.old, col.new) {
      g.o <- scale(row.old)
      g.n <- scale(row.new)
      c.o <- scale(col.old)
      c.n <- scale(col.new)
      res <- (colSums(g.o * g.n) / (nrow(g.o)-1) > cor.limit &
              colSums(c.o * c.n) / (nrow(c.o)-1) > cor.limit)
      res & !is.na(res)
    }
  }

  ## Initialize a couple of things
  iter <- 0
  index <- seq_len(ncol(all.seeds))
  if (oscillation) { fire <- character(no.seeds) }
  row.old <- all.seeds
  col.old <- matrix(NA, nrow=ncol(normed.data$Ec), ncol=no.seeds)
  row.res <- matrix(NA, nrow=nrow(normed.data$Ec), ncol=no.seeds)
  col.res <- matrix(NA, nrow=ncol(normed.data$Ec), ncol=no.seeds)
  
  ## Main loop starts here
  while (TRUE) {

    iter <- iter + 1
    one.step <- isa.step(normed.data, rows=row.old, thr.row=thr.row,
                         thr.col=thr.col, direction=direction)

    row.new <- one.step$rows
    col.new <- one.step$columns

    ## Mark converged seeds
    conv <- check.convergence(row.old=row.old, row.new=row.new,
                              col.old=col.old, col.new=col.new)

    ## Mark all zero seeds
    zero <- apply(row.new, 2, function(x) all(x==0))
    
    ## Mark oscillating ones, if requested
    if (oscillation && iter > 1) {
      new.fire <- apply(row.new, 2, function(x) sum(round(x, 4)))
      fire <- paste(sep=":", fire, new.fire)
      osc <- logical(ncol(row.new))
      osc[ (mat <- regexpr("(:.*:.*)\\1$", fire)) != -1] <- TRUE
      osc <- osc & !conv
      
      if (any(osc)) {
        mat <- cbind(mat[osc], attributes(mat, "match.length")[[1]][osc])
        mat <- sapply(seq(length=nrow(mat)), function(x) substr(fire[osc][x],
                            mat[x,1], mat[x,1]+mat[x,2]))
        mat <- sapply(mat, function(x) sum(utf8ToInt(x) == 58), USE.NAMES=FALSE )
        seeddata$oscillation[index[osc]] <- mat/2
      }
    } else {
      osc <- FALSE
    }
    
    ## These are all to throw out
    drop <- which(conv | zero | osc)
    
    ## Drop the seeds to be dropped
    if (length(drop) != 0) {
      row.res[,index[drop]] <- row.new[,drop]
      col.res[,index[drop]] <- col.new[,drop]
      seeddata$iterations[index[drop]] <- iter
      row.new <- row.new[,-drop,drop=FALSE]
      col.new <- col.new[,-drop,drop=FALSE]
      if (oscillation) { fire <- fire[-drop] }
      thr.row <- thr.row[-drop]
      thr.col <- thr.col[-drop]
      index <- index[-drop]
    }
    
    if (ncol(row.new)==0 || iter>maxiter) { break; }

    row.old <- row.new
    col.old <- col.new
    
  } ## End of main loop

  list(rows=row.res, columns=col.res,
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

isa.step <- function(normed.data, rows, thr.row, thr.col, direction) {

  Ec <- normed.data$Ec
  Eg <- normed.data$Eg

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

  if ("hasNA" %in% names(attributes(normed.data)) && !attr(normed.data, "hasNA")) {
    col.new <- filter(Eg %*% rows,    thr.col, direction[1])
    row.new <- filter(Ec %*% col.new, thr.row, direction[2])
  } else {
    col.new <- filter(na.multiply(Eg, rows   ), thr.col, direction[1])
    row.new <- filter(na.multiply(Ec, col.new), thr.row, direction[2])
  }

  list(columns=col.new, rows=row.new)
}

isa.fix.oscillation <- function(normed.data, isaresult) {

  if (!isaresult$rundata$oscillation) {
    stop("No oscillating modules were searched")
  }

  if (all(isaresult$seeddata$oscillation == 0) ) {
    return(isaresult)
  }

  rerun <- which(isaresult$seeddata$oscillation != 0)
  len <- isaresult$seeddata$oscillation[rerun]

  for (s in seq_along(rerun)) {
    res <- list(matrix(0, nr=nrow(isaresult$rows), nc=len[s]),
                matrix(0, nr=nrow(isaresult$columns), nc=len[s]))
    res[[1]][,1] <- isaresult$rows[,rerun[s]]
    res[[2]][,1] <- isaresult$columns[,rerun[s]]
    for (i in 1:(len[s]-1)) {
      tmp <- isa.step(normed.data, res[[1]][,i,drop=FALSE],
                      thr.row=isaresult$seeddata$thr.row[rerun[s]],
                      thr.col=isaresult$seeddata$thr.cond[rerun[s]],
                      direction=isaresult$rundata$direction)
      res[[1]][,i+1] <- tmp[[2]]
      res[[2]][,i+1] <- tmp[[1]]
    }
    chosen <- which.min(apply(res[[1]], 2, sum))
    isaresult$rows[,rerun[s]] <- res[[1]][,chosen]
    isaresult$columns[,rerun[s]] <- res[[2]][,chosen]
  }

  isaresult
}

isa.unique <- function(normed.data, isaresult, method=c("cor", "round"),
                       ignore.div=TRUE, cor.limit=0.99, neg.cor=TRUE,
                       drop.zero=TRUE) {

  method <- match.arg(method)

  if (ncol(isaresult$rows) == 0) { return(isaresult) }

  ## drop divergent seeds
  if (ignore.div) {
    invalid <- is.na(isaresult$seeddata$iterations)
    if (any(invalid)) {
      valid <- !invalid
      isaresult$rows <- isaresult$rows[,valid,drop=FALSE]
      isaresult$columns <- isaresult$columns[,valid,drop=FALSE]
      isaresult$seeddata <- isaresult$seeddata[valid,,drop=FALSE]
    }
  }
  if (ncol(isaresult$rows) == 0) { return(isaresult) }
  
  ## drop all zero seeds
  if (drop.zero) {
    valid <- apply(isaresult$rows, 2, function(x) any(x != 0))
    if (!all(valid)) {
      isaresult$rows <- isaresult$rows[,valid,drop=FALSE]
      isaresult$columns <- isaresult$columns[,valid,drop=FALSE]
      isaresult$seeddata <- isaresult$seeddata[valid,,drop=FALSE]
    }
  }
  if (ncol(isaresult$rows) == 0) { return(isaresult) }

  if (method=="cor") {
    if (neg.cor) { ABS <- abs } else { ABS <- function(x) x }
    cm <- pmin(ABS(cor(isaresult$rows)), ABS(cor(isaresult$columns)))
    cm[ lower.tri(cm, diag=TRUE) ] <- 0
    uni <- apply(cm < cor.limit, 2, all)
    freq <- apply(cm >= cor.limit, 1, sum)[uni] + 1
  } else if (method=="round") {
    ## TODO
  }

  isaresult$rows <- isaresult$rows[,uni,drop=FALSE]
  isaresult$columns <- isaresult$columns[,uni,drop=FALSE]

  isaresult$seeddata <- isaresult$seeddata[uni,,drop=FALSE]
  isaresult$seeddata$freq <- freq

  isaresult$rundata$unique <- TRUE

  isaresult      
}


isa.row.from.col <- function(normed.data, col.seeds, thr.col, direction) {


  Ec <- normed.data$Ec

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

  if ("hasNA" %in% names(attributes(normed.data)) && !attr(normed.data, "hasNA")) {
    row.new <- filter(Ec %*% col.seeds, thr.row, direction)
  } else {
    row.new <- filter(na.multiply(Ec, col.seeds), thr.row, direction)
  }

  row.new
}  

generate.seeds <- function(length, count=100, method=c("uni"),
                           sparsity, ...) {

  if (method == "uni") {
    if (missing(sparsity)) {
      sparsity <- sample(1:length, count, replace=TRUE)
    } else {
      sparsity <- rep(round(sparsity*length), length=count)
    }
    g <- matrix(0, nrow=length, ncol=count)
    for (i in 1:count) {
      g[sample(length, sparsity[i]),i] <- 1
    }
  }
  g
}
