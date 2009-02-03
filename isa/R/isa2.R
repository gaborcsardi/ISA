
normalize <- function (data, prenormalize = TRUE) 
{

  annotation <-  organism <- pData <- features <- NA
  
  if (is(data, "ExpressionSet")) {
    annotation <- annotation(data)
    pData <- pData(data)
    features <- featureNames(data)
    
    ann.package <- paste(sep="", annotation, ".db")
    require(ann.package, character.only=TRUE)
    organism <- get(paste(sep="", annotation, "ORGANISM"))
    data <- t(exprs(data))
  }
  
  dimnames(data) <- NULL
  Ec <- scale(data)
  if (prenormalize) {
    Eg <- scale(t(Ec))
  } else {
    Eg <- scale(t(data))
  }
  res <- list(t(Ec), t(Eg))
  attr(res, "hasNA") <- any(is.na(data))
  attr(res, "organism") <- organism
  attr(res, "annotation") <- annotation
  attr(res, "pData") <- pData
  attr(res, "features") <- features
  attr(res, "prenormalize") <- prenormalize
  res
}

isa <- function(normeddata, g=NULL, c=NULL, tc, tg=tc, down=c(TRUE, FALSE),
                convergence=c("cor", "loosy", "pos.genes", "eps",
                  "no.pos.genes", "no.pos.genes.m3"),
                eps=NULL, cor.limit=0.99,
                oscillation=TRUE, maxiter=100, miniter=0,
                dots=TRUE,
                threshold.type=c("data", "predef"),
                normalization=c("orig", "std", "stdthr"),
                organism=NULL, annotation=NULL, pData=NULL, features=NULL) {

  if (missing(g) && missing(c)) {
    stop("Give either the gene or the condition seeds")
  }

  if (!missing(g) && !missing(c)) {
    stop("Give only one of the gene or condition seeds")
  }

  if (missing(c)) { noseeds <- ncol(g) } else { noseeds <- ncol(c) }
  
  dimnames(normeddata[[1]]) <- NULL
  dimnames(normeddata[[2]]) <- NULL

  threshold.type <- match.arg(threshold.type)
  normalization <- match.arg(normalization)
  
  if (is.logical(down)) {
    down <- c("up", "updown")[down+1]
  }
  if (any(!down %in% c("up", "down", "updown"))) {
    stop("Invalid `down' argument")
  }
  
  orig.tg <- tg ; orig.tc <- tc
  if (length(tc) != 1 && length(tc) != noseeds) {
    stop("`tc' is invalid")
  }
  if (length(tg) != 1 && length(tg) != noseeds) {
    stop("`tg' is invalid")
  }

  if (is.null(organism)) organism <- attr(normeddata, "organism")
  if (is.null(annotation)) annotation <- attr(normeddata, "annotation")
  if (is.null(pData)) pData <- attr(normeddata, "pData")
  if (is.null(features)) features <- attr(normeddata, "features")

  if (missing(g)) {
    g <- isa.gene.from.cond(normeddata, c=c, tc=tc, tg=tg,
                            down=down, threshold.type=threshold.type,
                            normalization=normalization)
  }
  
  rundata <- list(down=down, eps=eps, cor.limit=cor.limit, maxiter=maxiter,
                  N=max(ncol(g), 0), convergence=convergence,
                  normalization=normalization,
                  organism=organism, annotation=annotation,
                  pData=pData, features=features,
                  prenormalize=attr(normeddata, "prenormalize"),
                  unique=FALSE, miniter=miniter,
                  threshold.type=threshold.type,
                  oscillation=oscillation)

  if (!is.matrix(g)) { g <- matrix(g, nc=1) }  
  if (length(g) == 0) {
    return(list(genes=g, conditions=NA,
                seeddata=data.frame(iterations=numeric(),
                  oscillation=logical(), freq=numeric()),
                rundata=rundata))
  }
  
  N <- ncol(g)
  iter <- 0
  result1 <- g
  result1[] <- NA
  result2 <- matrix(NA, ncol(normeddata[[1]]), ncol=ncol(g))
  prev <- list(result1, result2)
  index <- seq(ncol(g))
  iterresult <- rep(NA, ncol(g))
  fire <- character(N)
  if (oscillation) {
    oscresult <- numeric(ncol(g))
  } else {
    oscresult <- NA
  }

  down <- rep(down, length=2)

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
              apply(cond-prev[[2]], 2, function(x) all(abs(x)<eps)))
      res & !is.na(res)
    }
  } else if (convergence=="loosy") {
    check.convergence <- function() {
      apply(g,2,function(x) sum(x>0)) == apply(prev[[1]], 2, function(x) sum(x>0))
    }
  } else if (convergence=="no.pos.genes") {
    check.convergence <- function() {
      apply(g>0, 2, sum) == apply(prev[[1]]>0, 2, sum)
    }
  } else if (convergence=="no.pos.genes.m3") {
    no.pos <- numeric()
    check.convergence <- function() {
      no.pos[,iter-2] == no.pos[,iter+1]
    }
  } else if (convergence=="cor") {
    check.convergence <- function() {
      g <- scale(g)
      g2 <- scale(prev[[1]])
      cond <- scale(cond)
      cond2 <- scale(prev[[2]])
      res <- (colSums(g*g2) / (nrow(g)-1) > cor.limit &
              colSums(cond*cond2) / (nrow(cond)-1) > cor.limit)
      res & !is.na(res)
    }
  }    
  
  ## 
  
  while (TRUE) {
    if (dots) cat(".")
    iter <- iter+1
    res <- isa.step(normeddata, g, tc=tc, tg=tg, down=down,
                    threshold.type=threshold.type, normalization=normalization)
    g <- res[[2]]
    cond <- res[[1]]

    if (iter > miniter) {
      
      ## Throw out converged vectors
      if (convergence=="no.pos.genes.m3") {
        no.pos <- cbind(no.pos, apply(g>0, 2, sum))
      }
      conv <- check.convergence()
      conv <- conv | is.na(conv)
      
      ## Throw out zero vectors
      zero <- apply(g, 2, function(x) all(x==0))
      
      ## Throw out oscillating vectors
      if (oscillation && iter>1) {
        new.fire <- apply(g, 2, function(x) sum(round(x, 4)))
        fire <- paste(sep=":", fire, new.fire)
        osc <- logical(ncol(g))
        osc[ (mat <- regexpr("(:.*:.*)\\1$", fire)) != -1] <- TRUE
        osc <- osc & !conv
        
        if (any(osc)) {
          mat <- cbind(mat[osc], attributes(mat, "match.length")[[1]][osc])
          mat <- sapply(seq(length=nrow(mat)), function(x) substr(fire[osc][x], mat[x,1], mat[x,1]+mat[x,2]))
          mat <- sapply(mat, function(x) sum(utf8ToInt(x) == 58), USE.NAMES=FALSE )
          oscresult[index[osc]] <- mat/2
          drop <- conv | zero | osc
        } else {
          drop <- conv | zero
        }
      } else {
        drop <- conv | zero
      }
      
      drop <- which(drop)
      if (length(drop) != 0) {
        result1[,index[drop]] <- g[,drop]
        result2[,index[drop]] <- res[[1]][,drop]
        iterresult[index[drop]] <- iter
        g <- g[,-drop,drop=FALSE]
        cond <- cond[,-drop,drop=FALSE]
        if (oscillation) {
          fire <- fire[-drop]
        }
        if (length(tc)!=1) {
          tc <- tc[-drop]
        }
        if (length(tg)!=1) {
          tg <- tg[-drop]
        }
        index <- index[-drop]
      }
      if (ncol(g)==0) { break }
    }
    
    ## Check for maxiter
    if (iter >= maxiter) { break }
    
    prev <- list(g, cond)
  }
  result1[,index] <- NA  
  
  if (dots) cat("\n")
  list(genes=result1, conditions=result2,
       seeddata=data.frame(iterations=iterresult,
         oscillation=oscresult, freq=1, tg=orig.tg, tc=orig.tc),
       rundata=rundata)
}

isa.merge <- function(..., FORCE=FALSE) {
  args <- list(...)
  if (length(args)==1) {
    return(args[[1]])
  }

  force.fun <- function(all.data) {
    ## These fields can differ: tg, tc, down, maxiter, convergence, eps
    ## We don't hve to deal with oscillation, if it was "FALSE", then
    ## seeddata$oscillation contains NA's only
    may.differ <- c("tg", "tc", "down", "maxiter", "convergence", "eps")
    for (n in may.differ) {
      vals <- unique(lapply(all.data, function(x) x$rundata[[n]]))
      if (length(vals)==1) next
      ## add it to seeddata
      for (i in seq(along=all.data)) {
        all.data[[i]]$seeddata[[n]] <- all.data[[i]]$rundata[[n]]
        all.data[[i]]$rundata[[n]] <- NA
      }
    }
    all.data
  }
  
  ## Check that rundata is the same
  exit <- FALSE
  for (i in seq(length=length(args)-1)) {
    if (exit) break
    rdata1 <- args[[i]]$rundata
    rdata2 <- args[[i+1]]$rundata
    if (length(rdata1) != length(rdata2) ||
        any(sort(names(rdata1)) != sort(names(rdata2))) ) {
      if (!FORCE) {
        stop("Different rundata fields for #", i, " and #", i+1)
      } else {
        args <- force.fun(args)
        exit <- TRUE
        break
      }
    }
    for (j in seq(length=length(rdata1))) {
      n <- names(rdata1)[j]
      if (n=="N") { next }              # We allow 'N' to be different
      if (! all(rdata1[[n]] == rdata2[[n]]) ) {
        if (!FORCE) {
          stop("Different rundata fields for #", i, " and #", i+1)
        } else {
          args <- force.fun(args)
          exit <- TRUE
          break
        }
      }
    }
  }

  ## They are compatible, do the merge
  res <- list()
  res$genes <- do.call(cbind, lapply(args, "[[", "genes"))
  res$conditions <- do.call(cbind, lapply(args, "[[", "conditions"))
  res$seeddata <- do.call(rbind, lapply(args, "[[", "seeddata"))
  res$rundata <- args[[1]]$rundata
  res$rundata$N <- sum(sapply(args, function(x) x$rundata$N))

  res
}

isa.fix.oscillation <- function(normeddata, isaresult) {

  ## Check the oscillating seeds and determine a common gene vector
  ## for those in the same oscillating attractors.
  ## For now, we just keep the gene vector that has the lowest
  ## sum over all its elements and the corresponding condition
  ## vector.

  ## Did we look for oscillating modules?
  if (! isaresult$rundata$oscillation) {
    stop("No oscillating modules were searched")
  }

  ## Are there any oscillating modules?
  if ( all(isaresult$seeddata$oscillation == 0) ) {
    return(isaresult)
  }
  
  rerun <- which(isaresult$seeddata$oscillation != 0)
  len <- isaresult$seeddata$oscillation[rerun]

  ## Run them one-by-one, for simplicity
  for (s in seq(along=rerun)) {
    res <- list( list(isaresult$conditions[,rerun[s]], isaresult$genes[,rerun[s]]) )
    for (i in 1:len[s]) {
      res[[i+1]] <- isa.step(normeddata, res[[i]][[2]],
                             tc=isaresult$rundata$tc, tg=isaresult$rundata$tg,
                             down=isaresult$rundata$down)
      ss <- sapply(res, function(x) sum(x[[2]]))
      chosen <- which.min(ss)
      isaresult$genes[,rerun[s]] <- res[[chosen]][[2]]
      isaresult$conditions[,rerun[s]] <- res[[chosen]][[1]]
    }
  }

  isaresult
}

isa.step <- function(normeddata, g, tc, tg=tc, down=c(TRUE, FALSE),
                     threshold.type=c("data", "predef"),
                     normalization=c("orig", "std", "stdthr")) {
                     

  Ec <- normeddata[[1]]
  Eg <- normeddata[[2]]

  if (is.logical(down)) {
    down <- c("up", "updown")[down+1]
  }
  if (any(!down %in% c("up", "down", "updown"))) {
    stop("Invalid `down' argument")
  }
  down <- rep(down, length=2)

  threshold.type <- match.arg(threshold.type)
  normalization <- match.arg(normalization)
  if (normalization=="std") { nmode <- 0 } else { nmode <- 1 }
  nmode <- as.integer(nmode)
  if (threshold.type == "data") {   
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
  } else if (threshold.type=="predef") {
    stop("`predef' type threshold is not implemented")
##     filter <- function(x, t, down) {
##       if (down) {
##         x <- .Call("beta_filter_updown_predef", x, as.double(t), PACKAGE="isa")
##       } else {
##         x <- .Call("beta_filter_up_predef", x, as.double(t), PACKAGE="isa")
##       }
##     }
  }

  hasNA <- attr(normeddata, "hasNA")
  if (!hasNA) {
    c2 <- filter(Eg %*% g, tc, down[1])
    g2 <- filter(Ec %*% c2, tg, down[2])
  } else {
    c2 <- filter(na.multiply(Eg, g), tc, down[1])
    g2 <- filter(na.multiply(Ec, c2), tg, down[2])
  }
  
  list(c2, g2)
}

isa.gene.from.cond <- function(normeddata, c, tc, tg, down,
                               threshold.type, normalization) {

  Ec <- normeddata[[1]]
  Eg <- normeddata[[2]]

  if (is.logical(down)) {
    down <- c("up", "updown")[down+1]
  }
  if (any(!down %in% c("up", "down", "updown"))) {
    stop("Invalid `down' argument")
  }
  down <- rep(down, length=2)

  if (normalization=="std") { nmode <- 0 } else { nmode <- 1 }
  nmode <- as.integer(nmode)
  if (threshold.type == "data") {   
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
  } else if (threshold.type=="predef") {
    stop("`predef' type threshold is not implemented")
##     filter <- function(x, t, down) {
##       if (down) {
##         x <- .Call("beta_filter_updown_predef", x, as.double(t), PACKAGE="isa")
##       } else {
##         x <- .Call("beta_filter_up_predef", x, as.double(t), PACKAGE="isa")
##       }
##     }
  }

  hasNA <- attr(normeddata, "hasNA")
  if (!hasNA) {
    g <- filter(Ec %*% c, tg, down[2])
  } else {
    g <- filter(na.multiply(Ec, c), tg, down[2])
  }
  
  g
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

isa.unique <- function(normeddata, isaresult, method=c("round", "cor"),
                       ignore.div=TRUE, digits=NA,
                       cor.cut=0.99, neg.cor=TRUE, drop.zero=TRUE) {

  method <- match.arg(method)
  
  if (ignore.div && ncol(isaresult$genes) != 0) {
    valid <- !is.na(isaresult$seeddata$iterations)
    isaresult$genes <- isaresult$genes[,valid,drop=FALSE]
    isaresult$conditions <- isaresult$conditions[,valid,drop=FALSE]
    isaresult$seeddata <- isaresult$seeddata[valid,,drop=FALSE]
  }
  
  if (drop.zero && ncol(isaresult$genes) != 0) {
    valid <- apply(isaresult$genes, 2, function(x) any(x != 0))
    isaresult$genes <- isaresult$genes[,valid,drop=FALSE]
    isaresult$conditions <- isaresult$conditions[,valid,drop=FALSE]
    isaresult$seeddata <- isaresult$seeddata[valid,,drop=FALSE]
  }

  if (ncol(isaresult$genes)==0) {
    return(isaresult)
  }
  
  if (method == "round") {
    
    if (!is.na(digits)) {
      genes <- structure(round(as.numeric(isaresult$genes), digits),
                         dim=dim(isaresult$genes))
    } else {
      genes <- structure(as.numeric(isaresult$genes != 0),
                         dim=dim(isaresult$genes))
    }

    a2 <- apply(genes, 2, paste, collapse=":")
    uni <- !duplicated(a2)
    freq <- sapply(a2[uni], function(x) sum( isaresult$seeddata$freq[a2 == x] ))
    
  } else if (method=="cor") {
    if (ncol(isaresult$genes)==0) {
      uni <- logical()
      freq <- numeric()
    } else {
      cm <- cor(isaresult$genes)
      cm[ lower.tri(cm, diag=TRUE) ] <- 0
      if (neg.cor) { cm <- abs(cm) }
      uni <- apply(cm < cor.cut, 2, all)
      freq <- apply(cm >= cor.cut, 1, sum)[uni] + 1
    }
  }

  genes <- isaresult$genes[,uni,drop=FALSE]
  conditions <- isaresult$conditions[,uni,drop=FALSE]
  
  seeddata <- isaresult$seeddata[uni,,drop=FALSE]
  seeddata$freq <- freq

  isaresult$rundata$unique <- TRUE
  list(genes=genes, conditions=conditions, seeddata=seeddata,
       rundata=isaresult$rundata)
       
}

generate.seeds <- function(length, count=100, method="uni", gs= NULL, ...) {

  known.methods <- c("uni")
  method <- pmatch(method, known.methods)
  if (is.na(method)) {
    stop(paste("Unknown method, must be one of `",
               paste(collapse=", ", known.methods), "'"))
  }
  if (method == 1) {                    # uni
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

sweep.1d <- function(nm, isalist, method=c("round", "cor"),
                     digits=NA, cor.cut=0.99, merge.edges=FALSE) {

  method <- match.arg(method)
  
  ## 1) it is expected that isa.unique was run on all entries in the list
  ## 2) either tc or tg should be constant for all runs in the list

  tg <- sapply(isalist, function(x) x$rundata$tg)
  tc <- sapply(isalist, function(x) x$rundata$tc)

  if (any(tg != tg[1]) && any(tc != tc[1])) {
    stop("Either `tg' or `tc' must be constant in the list")
  }

  if (all(tg==tg[1])) {
    key <- tc
  } else {
    key <- tg
  }

  isalist <- isalist[ order(key, decreasing=TRUE) ]
  
  if (method=="round") {
    
    ## We create a character representation from the genes
    if (is.na(digits)) {      
      code <- function(x) {
        genes <- structure(as.numeric(x$genes != 0), dim=dim(x$genes))
        apply(genes, 2, paste, collapse=":")
      }
    } else {
      code <- function(x) {
        genes <- structure(round(x$genes, digits),
                           dim=dim(x$genes))
        apply(genes, 2, paste, collapse=":")
      }
    }
                           
    genes <- lapply(isalist, code)
                           
  }

  conv.to <- list( numeric() )
  for (i in seq(along=isalist)[-1]) {

    ## Are there some genes at all?

    if (ncol(isalist[[i-1]]$genes) == 0) {
      conv.to[[i]] <- numeric()
      next
    }
    
    ## We run the modules from i-1 using the thresholds from i
    tmpres <- isa(nm, isalist[[i-1]]$genes,
                  tc=isalist[[i]]$rundata$tc, tg=isalist[[i]]$rundata$tg,
                  down=isalist[[i]]$rundata$down,
                  convergence=isalist[[i]]$rundata$convergence,
                  eps=isalist[[i]]$rundata$eps,
                  oscillation=isalist[[i]]$rundata$oscillation,
                  maxiter=isalist[[i]]$rundata$maxiter)
    if (tmpres$rundata$oscillation) {
      tmpres <- isa.fix.oscillation(nma, tmpres)
    }

    ## Do something with non-convergent ones
    tmpres$genes[ is.na(tmpres$genes) ] <- 0
    tmpres$conditions[ is.na(tmpres$conditions) ] <- 0
    
    ures <- isa.unique(normeddata, tmpres, method=method,
                       ignore.div=TRUE, digits=digits,
                       cor.cut=cor.cut, drop.zero=TRUE)

    if (method=="round") {

      ## add the newly found genes
      
      new.genes <- code(ures)
      first <- ! new.genes %in% genes[[i]]
      genes[[i]] <- c(genes[[i]], new.genes[first])
      isalist[[i]]$genes <- cbind(isalist[[i]]$genes,
                                  ures$genes[,first,drop=FALSE])
      isalist[[i]]$conditions <- cbind(isalist[[i]]$conditions,
                                       ures$conditions[,first,drop=FALSE])
      isalist[[i]]$seeddata <- rbind(isalist[[i]]$seeddata,
                                     ures$seeddata[first,,drop=FALSE])

      ## check what converged to what
      
      conv.to[[i]] <- match(code(tmpres), genes[[i]])
      
    } else if (method=="cor") {
      
      ## add the newly found genes, if any
      if (ncol(ures$genes) != 0 && ncol(isalist[[i]]$genes) != 0) {
        cm <- cor(isalist[[i]]$genes, ures$genes)
        first <- apply(cm<cor.cut, 2, all)
        isalist[[i]]$genes <- cbind(isalist[[i]]$genes,
                                    ures$genes[,first,drop=FALSE])
        isalist[[i]]$conditions <- cbind(isalist[[i]]$conditions,
                                         ures$conditions[,first,drop=FALSE])
        isalist[[i]]$seeddata <- rbind(isalist[[i]]$seeddata,
                                       ures$seeddata[first,,drop=FALSE])
      }
      
      ## check what converged to what

      if (ncol(isalist[[i]]$genes) != 0 && ncol(tmpres$genes) != 0) {
        cm <- cor(isalist[[i]]$genes, tmpres$genes)
        conv.to[[i]] <- apply(cm, 2, function(x) which(x >= cor.cut)[1])
      } else {
        conv.to[[i]] <- numeric()
      }
    }

  } ## for

  level <- as.list( rep(seq(along=isalist), sapply(isalist,
                              function(x) ncol(x$genes))) )
  allgenes <- matrix(unlist(lapply(isalist, "[[", "genes")),
                     nrow=nrow(isalist[[1]]$genes))
  allconditions <- matrix(unlist(lapply(isalist, "[[", "conditions")),
                     nrow=nrow(isalist[[1]]$conditions))

  if (method=="round") {
    genes <- unlist(genes)
  }

  from <- seq(length=sum(sapply(conv.to, length)))
  v <- 0; for (i in seq(along=conv.to)) {
    v <- v + length(conv.to[[i]])
    conv.to[[i]] <- conv.to[[i]] + v
  }
  to <- unlist(conv.to)
  from <- from[ !is.na(to) ]
  to <- to[ !is.na(to) ]

  ## Do merges along the edges

  changed <- merge.edges
  while (changed) {
    if (ncol(allgenes)<=1 || length(from)==0) { break }
    
    changed <- FALSE
    ## Filter out those which are connected by an edge and are the same
    if (method=="round") {
      same <- which(genes[from] == genes[to])
    } else if (method=="cor") {
      same <- which(sapply(seq(length(from)), function(x)
                           all(( sign(allgenes[,from[x]]) ) ==
                               ( sign(allgenes[,to[x]]) )) &&
                           cor(allgenes[,from[x]], allgenes[,to[x]]) > cor.cut))
    }
    
    for (i in seq(along=same)) {
      changed <- TRUE
      g1 <- min(from[same[i]], to[same[i]])
      g2 <- max(from[same[i]], to[same[i]])
      ## remove the gene
      allgenes <- allgenes[,-g2, drop=FALSE]
      allconditions <- allconditions[,-g2, drop=FALSE]
      if (method=="round") { genes <- genes[-g2] }
      level[[g1]] <- unique( c(level[[g1]], level[[g2]]) )      
      level <- level[-g2]
      ## rewrite the from, to vectors
      from[ from==g2 ] <- g1
      to[ to==g2 ] <- g1
      from[ from>g2 ] <- from[ from>g2 ] - 1
      to[ to>g2 ] <- to[ to>g2 ] - 1
    }
    ## remove loop edges
    nonloop <- from != to
    from <- from[nonloop]
    to <- to[nonloop]
  }
  
  list(genes=allgenes, conditions=allconditions,
       edges=cbind(from, to), level=level, annotation=isalist[[1]]$annotation)
}

## Extract the genes in the TMs, this is not trivial because of the
## oscillation modules. For these, the gene score is the average
## over all gene vectors that appear during the oscillation.
## The same is done for the conditions

extract.genes <- function(normeddata, isaresult) {

  rerun <- which(isaresult$seeddata$oscillation != 0)
  len <- isaresult$seeddata$oscillation[rerun]

  ## Run them one-by-one, for simplicity
  for (s in seq(along=rerun)) {
    res <- list( list(isaresult$conditions[,rerun[s]], isaresult$genes[,rerun[s]]) )
    for (i in 1:len[s]) {
      res[[i+1]] <- isa.step(normeddata, res[[i]][[2]],
                             tc=isaresult$rundata$tc, tg=isaresult$rundata$tg,
                             down=isaresult$rundata$down)

      tmp <- sapply(res, function(x) x[[2]])
      isaresult$genes[,rerun[s]] <- apply(tmp, 1, mean)
      tmp <- sapply(res, function(x) x[[1]])
      isaresult$conditions[,rerun[s]] <- apply(tmp, 1, mean)
    }
  }  

  isaresult
}

