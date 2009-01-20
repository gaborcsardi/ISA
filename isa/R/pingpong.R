
ppa.normalize <- function(data, prenormalize=TRUE) {

  dimnames(data[[1]]) <- NULL
  dimnames(data[[2]]) <- NULL

  if (prenormalize) {
    Egc <- t(scale(t( scale(data[[1]]))))
  } else {
    Egc <- t(scale(t(data[[1]])))
  }
  Ecg <- t(scale(data[[1]]))
  Ecd <- t(scale(data[[2]]))
  if (prenormalize) {
    Edc <- t(scale(t(scale(data[[2]]))))
  } else {
    Edc <- t(scale(t(data[[2]])))
  }

  res <- list(Egc, Ecd, Edc, Ecg)
  attr(res, "hasNA") <- c(any(is.na(Egc)) | any(is.na(Ecd)), 
                          any(is.na(Edc)) | any(is.na(Ecg)))
  res
} 

ping <- function(normeddata, g, tc, td=tc,
                 down=c(TRUE, FALSE),
                 normalization=c("orig", "std", "stdthr") ) {

  if (!is.matrix(g)) { g <- matrix(g, nc=1) }
  dimnames(normeddata[[1]]) <- NULL
  dimnames(normeddata[[2]]) <- NULL
  dimnames(normeddata[[3]]) <- NULL
  dimnames(normeddata[[4]]) <- NULL

  if (length(tc) != 1 && length(tc) != ncol(g)) {
    stop("`tc' is invalid")
  }
  if (length(td) != 1 && length(td) != ncol(g)) {
    stop("`td' is invalid")
  }

  Egc <- normeddata[[1]]
  Ecd <- normeddata[[2]]

  if (is.logical(down)) {
    down <- c("up", "updown")[down+1]
  }
  down <- rep(down, length=2)

  normalization <- match.arg(normalization)
  if (normalization=="std") { nmode <- 0 } else { nmode <- 1 }
  nmode <- as.integer(nmode)
  
  filter <- function(x, t, down) {
    t <- rep(t, length=ncol(x))
    if (down=="updown") {
      if (normalization=="orig") {
        x <- .Call("beta_filter_updown_vart", x, as.double(t),
                   PACKAGE="isa")
      } else {
        x <- .Call("beta_filter_updown_vart_nn", x, as.double(t), nmode,
                   PACKAGE="isa")
      }
    } else if (down=="up") {
      if (normalization=="orig") {
        x <- .Call("beta_filter_up_vart", x, as.double(t),
                   PACKAGE="isa")
      } else {
        x <- .Call("beta_filter_up_vart_nn", x, as.double(t), nmode,
                   PACKAGE="isa")
      }
    } else { ## down
      if (normalization=="orig") {
        x <- .Call("beta_filter_down_vart", x, as.double(t),
                   PACKAGE="isa")
      } else {
        x <- .Call("beta_filter_down_vart_nn", x, as.double(t), nmode,
                   PACKAGE="isa")
      }
    }      
    x
  }

  hasNA <- attr(normeddata, "hasNA")
  if (!hasNA[[1]]) {
    c2 <- filter(Egc %*% g, tc, down[1])
    d2 <- filter(Ecd %*% c2, td, down[2])
  } else {
    c2 <- filter(na.multiply(Egc, g), tc, down[1])
    d2 <- filter(na.multiply(Ecd, c2), td, down[2])
  }

  list(c2, d2)
}

pong <- function(normeddata, d, tc, tg=tc,
                 down=c(TRUE, FALSE),
                 normalization=c("orig", "std", "stdthr") ) {

  if (!is.matrix(d)) { d <- matrix(d, nc=1) }
  dimnames(normeddata[[1]]) <- NULL
  dimnames(normeddata[[2]]) <- NULL
  dimnames(normeddata[[3]]) <- NULL
  dimnames(normeddata[[4]]) <- NULL

  if (length(tc) != 1 && length(tc) != ncol(g)) {
    stop("`tc' is invalid")
  }
  if (length(td) != 1 && length(td) != ncol(g)) {
    stop("`td' is invalid")
  }

  Edc <- normeddata[[3]]
  Ecg <- normeddata[[4]]

  if (is.logical(down)) {
    down <- c("up", "updown")[down+1]
  }
  down <- rep(down, length=2)

  normalization <- match.arg(normalization)
  if (normalization=="std") { nmode <- 0 } else { nmode <- 1 }
  nmode <- as.integer(nmode)
  
  filter <- function(x, t, down) {
    t <- rep(t, length=ncol(x))
    if (down=="updown") {
      if (normalization=="orig") {
        x <- .Call("beta_filter_updown_vart", x, as.double(t),
                   PACKAGE="isa")
      } else {
        x <- .Call("beta_filter_updown_vart_nn", x, as.double(t), nmode,
                   PACKAGE="isa")
      }
    } else if (down=="up") {
      if (normalization=="orig") {
        x <- .Call("beta_filter_up_vart", x, as.double(t),
                   PACKAGE="isa")
      } else {
        x <- .Call("beta_filter_up_vart_nn", x, as.double(t), nmode,
                   PACKAGE="isa")
      }
    } else { ## down
      if (normalization=="orig") {
        x <- .Call("beta_filter_down_vart", x, as.double(t),
                   PACKAGE="isa")
      } else {
        x <- .Call("beta_filter_down_vart_nn", x, as.double(t), nmode,
                   PACKAGE="isa")
      }
    }      
    x
  }

  hasNA <- attr(normeddata, "hasNA")
  if (!hasNA[[1]]) {
    c2 <- filter(Edc %*% d, tc, down[1])
    g2 <- filter(Ecg %*% c2, tg, down[2])
  } else {
    c2 <- filter(na.multiply(Edc, d), tc, down[1])
    g2 <- filter(na.multiply(Ecg, c2), tg, down[2])
  }

  list(c2, g2)
}

ppa <- function(normeddata, g, tc, tg=tc, td=tc,
                down=c(TRUE, FALSE, TRUE, FALSE),
                convergence=c("loosy", "pos.genes", "eps"), eps=NULL,
                oscillation=TRUE, maxiter=100, dots=TRUE,
                annotation=NULL, threshold.type=c("data", "predef"),
                normalization=c("orig", "std", "stdthr")) {

  if (!is.matrix(g)) { g <- matrix(g, nc=1) }
  dimnames(normeddata[[1]]) <- NULL
  dimnames(normeddata[[2]]) <- NULL
  dimnames(normeddata[[3]]) <- NULL
  dimnames(normeddata[[4]]) <- NULL
  
  orig.tg <- tg ; orig.tc <- tc ; orig.td <- td
  if (length(tc) != 1 && length(tc) != ncol(g)) {
    stop("`tc' is invalid")
  }
  if (length(tg) != 1 && length(tg) != ncol(g)) {
    stop("`tg' is invalid")
  }
  if (length(td) != 1 && length(td) != ncol(g)) {
    stop("`td' is invalid")
  }

  if (length(g) == 0) {
    return(list(genes=g, conditions=NA, drugs=NA,
                seeddata=data.frame(iterations=numeric(),
                  oscillation=logical(), freq=numeric()),
                rundata=list(tg=tg, tc=tc, td=td, down=down,
                  eps=eps, maxiter=maxiter, N=0, convergence=convergence)))
  }
  
  N <- ncol(g)
  iter <- 0
  result1 <- g
  result1[] <- NA
  result2 <- matrix(NA, ncol(normeddata[[2]]), ncol=ncol(g))
  result3 <- matrix(NA, nrow(normeddata[[2]]), ncol=ncol(g))
  prev <- list(result1, result3)
  index <- seq(ncol(g))
  iterresult <- rep(NA, ncol(g))
  fire <- character(N)
  if (oscillation) {
    oscresult <- numeric(ncol(g))
  } else {
    oscresult <- NA
  }

  if (is.logical(down)) {
    down <- c("up", "updown")[down+1]
  }
  down <- rep(down, length=4)
                             
  ## Choose convergence checking function
  convergence=match.arg(convergence)
  if (convergence=="pos.genes") {
    check.convergence <- function() {
      apply( (g>0) == (prev[[1]]>0), 2, all)      
    }
  } else if (convergence=="eps") {
    if (missing(eps)) {
      stop("`eps' convergence check chosen, but no `eps' argument")
    }
    check.convergence <- function() {
      res <- (apply(g-prev[[1]], 2, function(x) all(abs(x)<eps)) &
              apply(d-prev[[2]], 2, function(x) all(abs(x)<eps)))
      res & !is.na(res)
    }
  } else if (convergence=="loosy") {
    check.convergence <- function() {
      apply(g,2,function(x) sum(x>0)) == apply(prev[[1]], 2, function(x) sum(x>0))
    }
  }

  ##

  while (TRUE) {
    if (dots) cat(".")
    iter <- iter+1
    res <- ppa.step(normeddata, g, tc=tc, tg=tg, td=td, down=down,
                    threshold.type=threshold.type, normalization=normalization)

    g <- res[[2]]
    d <- res[[3]]

    ## Throw out converges vectors
    conv <- check.convergence()
    conv <- conv | is.na(conv)

    ## Throw out zero vectors
    zero <- apply(g, 2, function(x) all(x==0))

    ## Throw out oscillating vectors
    ## TODO
    drop <- conv | zero

    drop <- which(drop)
    if (length(drop) != 0) {
      result1[,index[drop]] <- g[,drop]
      result2[,index[drop]] <- res[[1]][,drop]
      result3[,index[drop]] <- res[[3]][,drop]      
      iterresult[index[drop]] <- iter
      g <- g[,-drop,drop=FALSE]
      d <- d[,-drop,drop=FALSE]
      if (oscillation) {
        fire <- fire[-drop]
      }
      if (length(tc)!=1) {
        tc <- tc[-drop]
      }
      if (length(tg)!=1) {
        tg <- tg[-drop]
      }
      if (length(td)!=1) {
        td <- td[-drop]
      }
      index <- index[-drop]
    }
    if (ncol(g)==0) { break }
    
    ## Check for maxiter
    if (iter >= maxiter) { break }

    prev <- list(g,d)
  }
  result1[,index] <- NA

  if (dots) cat("\n")
  list(genes=result1, conditions=result2, drugs=result3,
       seeddata=data.frame(iterations=iterresult,
         oscillation=oscresult, freq=1, tg=orig.tg, tc=orig.tc, td=orig.td),
       rundata=list(down=down, oscillation=oscillation,
         maxiter=maxiter, N=N, convergence=convergence, eps=eps,
         annotation=annotation))
}

ppa.step <- function(normeddata, g, tc, tg=tc, td=tc,
                     down=c(TRUE, FALSE, TRUE, FALSE),
                     threshold.type=c("data", "predef"),
                     normalization=c("orig", "std", "stdthr")) {

  Egc <- normeddata[[1]]
  Ecd <- normeddata[[2]]
  Edc <- normeddata[[3]]
  Ecg <- normeddata[[4]]

  if (is.logical(down)) {
    down <- c("up", "updown")[down+1]
  }
  down <- rep(down, length=4)

  threshold.type <- match.arg(threshold.type)
  normalization <- match.arg(normalization)
  if (normalization=="std") { nmode <- 0 } else { nmode <- 1 }
  nmode <- as.integer(nmode)
  if (threshold.type != "data") {
    stop("Only `data' type of thresholding is supported")
  }
  
  filter <- function(x, t, down) {
    t <- rep(t, length=ncol(x))
    if (down=="updown") {
      if (normalization=="orig") {
        x <- .Call("beta_filter_updown_vart", x, as.double(t),
                   PACKAGE="isa")
      } else {
        x <- .Call("beta_filter_updown_vart_nn", x, as.double(t), nmode,
                   PACKAGE="isa")
      }
    } else if (down=="up") {
      if (normalization=="orig") {
        x <- .Call("beta_filter_up_vart", x, as.double(t),
                   PACKAGE="isa")
      } else {
        x <- .Call("beta_filter_up_vart_nn", x, as.double(t), nmode,
                   PACKAGE="isa")
      }
    } else { ## down
      if (normalization=="orig") {
        x <- .Call("beta_filter_down_vart", x, as.double(t),
                   PACKAGE="isa")
      } else {
        x <- .Call("beta_filter_down_vart_nn", x, as.double(t), nmode,
                   PACKAGE="isa")
      }
    }      
    x
  }

  hasNA <- attr(normeddata, "hasNA")
  if (!hasNA[[1]]) {
    c2 <- filter(Egc %*% g, tc, down[1])
    d2 <- filter(Ecd %*% c2, td, down[2])
  } else {
    c2 <- filter(na.multiply(Egc, g), tc, down[1])
    d2 <- filter(na.multiply(Ecd, c2), td, down[2])
  }

  if (!hasNA[[2]]) {
    c3 <- filter(Edc %*% d2, tc, down[3])
    g2 <- filter(Ecg %*% c3, tg, down[4])
  } else {
    c3 <- filter(na.multiply(Edc, d2), tc, down[3])
    g2 <- filter(na.multiply(Ecg, c3), tg, down[4])
  }

  list(c3, g2, d2)
}
    
ppa.unique <- function(normeddata, pparesult, method=c("round", "cor"),
                       ignore.div=TRUE, digits=NA,
                       cor.cut=0.99, drop.zero=TRUE) {

  method <- match.arg(method)
  
  if (ignore.div && ncol(pparesult$genes) != 0) {
    valid <- !is.na(pparesult$seeddata$iterations)
    pparesult$genes <- pparesult$genes[,valid,drop=FALSE]
    pparesult$conditions <- pparesult$conditions[,valid,drop=FALSE]
    pparesult$drugs <- pparesult$drugs[,valid,drop=FALSE]
    pparesult$seeddata <- pparesult$seeddata[valid,,drop=FALSE]
  }
  
  if (drop.zero && ncol(pparesult$genes) != 0) {
    valid <- apply(pparesult$genes, 2, function(x) any(x != 0))
    pparesult$genes <- pparesult$genes[,valid,drop=FALSE]
    pparesult$conditions <- pparesult$conditions[,valid,drop=FALSE]
    pparesult$drugs <- pparesult$drugs[,valid,drop=FALSE]
    pparesult$seeddata <- pparesult$seeddata[valid,,drop=FALSE]
  }

  if (ncol(pparesult$genes)==0) {
    return(pparesult)
  }
  
  if (method == "round") {
    
    if (!is.na(digits)) {
      genes <- structure(round(as.numeric(pparesult$genes), digits),
                         dim=dim(pparesult$genes))
    } else {
      genes <- structure(as.numeric(pparesult$genes != 0),
                         dim=dim(pparesult$genes))
    }

    a2 <- apply(genes, 2, paste, collapse=":")
    uni <- !duplicated(a2)
    freq <- sapply(a2[uni], function(x) sum( pparesult$seeddata$freq[a2 == x] ))
    
  } else if (method=="cor") {
    if (ncol(pparesult$genes)==0) {
      uni <- logical()
      freq <- numeric()
    } else {
      cm <- cor(pparesult$genes)
      cm[ lower.tri(cm, diag=TRUE) ] <- 0
      uni <- apply(cm < cor.cut, 2, all)
      freq <- apply(cm >= cor.cut, 1, sum)[uni] + 1
    }
  }

  genes <- pparesult$genes[,uni,drop=FALSE]
  conditions <- pparesult$conditions[,uni,drop=FALSE]
  drugs <- pparesult$drugs[,uni,drop=FALSE]
  
  seeddata <- pparesult$seeddata[uni,,drop=FALSE]
  seeddata$freq <- freq
  
  list(genes=genes, conditions=conditions, drugs=drugs, seeddata=seeddata,
       rundata=pparesult$rundata)
       
}
