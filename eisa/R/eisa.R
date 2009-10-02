
isa.result.to.ISAModules <- function(modules, data) {

  new.modules <- new("ISAModules",
                     genes=modules$rows,
                     conditions=modules$columns,
                     seeddata=modules$seeddata,
                     rundata=modules$rundata)

  new.modules@rundata$annotation <- annotation(data)
  library(paste(sep="", annotation(data), ".db"), character.only=TRUE)
  new.modules@rundata$organism <- get(paste(sep="", annotation(data),
                                            "ORGANISM"))

  new.modules@rundata$pData <- pData(data)

  rownames(new.modules@genes) <- featureNames(data)
  rownames(new.modules@conditions) <- sampleNames(data)
  
  new.modules
}

ISAModules.to.isa.result <- function(isaresult) {

  list(rows=isaresult@genes,
       columns=isaresult@conditions,
       seeddata=isaresult@seeddata,
       rundata=isaresult@rundata)

} 

eisa.get.nm <- function(data, modules) {
  if (!is.null(modules)) { data <- data[featureNames(modules),] }
  if (is(data, "ISAExpressionSet")) {
    data
  } else {
    if (!is.null(modules)) {
      pre <- runData(modules)$prenormalize
      if (is.null(pre)) { pre <- FALSE }
      ISANormalize(data, prenormalize=pre)
    } else {
      ISANormalize(data)
    }
  }
}

ISA <- function(data,
                 flist=filterfun(function(x) IQR(x) > 0.5),
                 uniqueEntrez=TRUE,
                 thr.gene=seq(2,4,by=0.5), thr.cond=seq(1,3,by=0.5),
                 no.seeds=100) {

  isa2:::isa.status("ISA on an ExpressionSet", "in")
  
  if (!is(data, "ExpressionSet")) {
    stop("Please supply an ExpressionSet object")
  }

  if (is.function(flist)) {
    library(genefilter)
    selected <- genefilter(data, flist)
    data <- data[selected,]
  } else if (! is.na(flist)) {
    stop("Could not interpret `flist' argument, should be a function or `NA'")
  }    

  if (uniqueEntrez) {
    library(paste(sep="", annotation(data), ".db"), character.only=TRUE)
    ENTREZ <- get(paste(sep="", annotation(data), "ENTREZID"))
    entrez <- mget(featureNames(data), ENTREZ)
    keep <- sapply(entrez, function(x) length(x) > 1 || !is.na(x))
    data <- data[keep,]
    vari <- apply(exprs(data), 1, var)
    larg <- findLargest(featureNames(data), 
                        vari, data=annotation(data))
    data <- data[larg,]
  }
  
  modules <- isa2::isa(exprs(data), thr.row=thr.gene, thr.col=thr.cond,
                      no.seeds=no.seeds)

  modules <- isa.result.to.ISAModules(modules, data)

  isa2:::isa.status("DONE", "out")
  
  modules
}

ISANormalize <- function(data, prenormalize=FALSE) {

  isa2:::isa.status("Normalizing ExpressionSet", "in")
  
  data <- as(data, "ISAExpressionSet")
  normed.data.exprs <- isa2::isa.normalize(exprs(data),
                                          prenormalize=prenormalize)

  data@assayData <- assayDataNew(exprs=exprs(data),
                                 er.exprs=t(normed.data.exprs$Er),
                                 ec.exprs=normed.data.exprs$Ec)

  data@prenormalized <- attr(normed.data.exprs, "prenormalize")
  data@hasNA <- attr(normed.data.exprs, "hasNA")

  isa2:::isa.status("DONE", "out")

  data
}

ISAIterate <- function(data, feature.seeds, sample.seeds,
                       thr.feat, thr.samp=thr.feat, ...) {
  
  isa2:::isa.status("Starting ISA iteration on an ExpressionSet", "in")

  data <- eisa.get.nm(data, modules=NULL)
  
  nm <- list(Er=t(featExprs(data)),
             Ec=sampExprs(data))

  attr(nm, "prenormalize") <- prenormalized(data)
  attr(nm, "hasNA") <- hasNA(data)

  if (!missing(feature.seeds) && !missing(sample.seeds)) {
    modules <- isa.iterate(nm, row.seeds=feature.seeds,
                           col.seeds=sample.seeds,
                           thr.row=thr.feat, thr.col=thr.samp, ...)
  } else if (!missing(feature.seeds)) {
    modules <- isa.iterate(nm, row.seeds=feature.seeds,
                           thr.row=thr.feat, thr.col=thr.samp, ...)
  } else if (!missing(sample.seeds)) {
    modules <- isa.iterate(nm, col.seeds=sample.seeds,
                           thr.row=thr.feat, thr.col=thr.samp, ...)
  } else {
    stop("Please give either some feature or some sample seeds")
  }
    
  modules <- isa.result.to.ISAModules(modules, data)
  
  isa2:::isa.status("DONE", "out")
  
  modules
}

ISAUnique <- function(data, isaresult, ...) {

  isa2:::isa.status("Creating unique ISA module set for expression data", "in")
  
  data <- eisa.get.nm(data, isaresult)
  
  nm <- list(Er=t(featExprs(data)),
             Ec=sampExprs(data))

  res <- ISAModules.to.isa.result(isaresult)

  res <- isa2::isa.unique(nm, res, ...)

  modules <- isa.result.to.ISAModules(res, data)

  isa2:::isa.status("DONE", "out")
  
  modules
}

ISASweep <- function(expset, modules, ...) {

  isa2:::isa.status("Sweeping an ExpressionSet")

  data <- eisa.get.nm(expset, modules)
    
  mat.data <- exprs(data)
  nm <- list(Er=t(featExprs(data)),
             Ec=sampExprs(data))
  res <- ISAModules.to.isa.result(modules)

  res <- isa.sweep(mat.data, nm, res, ...)

  modules <- isa.result.to.ISAModules(res, data)

  isa2:::isa.status("DONE", "out")

  modules
}

ISASweepGraph <- function(sweep.result) {
  res <- ISAModules.to.isa.result(sweep.result)
  G <- sweep.graph(res)
  V(G)$noFeatures <- V(G)$rows
  V(G)$noSamples  <- V(G)$cols
  G <- remove.vertex.attribute(G, "rows")
  G <- remove.vertex.attribute(G, "cols")
  G
}

ISASweepGraphPlot <- function(graph, vertex.label=V(graph)$id,
                              vertex.label.topleft=NA,
                              vertex.label.topright=NA,
                              vertex.label.bottomleft=NA,
                              vertex.label.bottomright=NA,
                              vertex.label.cex=0.8,
                              edge.label=NA, asp=FALSE, rescale=FALSE,
                              xlim=range(graph$layout[,1]),
                              ylim=range(graph$layout[,2]),
                              thresholds=TRUE,
                              xlab=NA, ylab=NA,
                              ...) {

  plot(NA, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, axes=FALSE)
  
  if (thresholds) {
    la <- sort(unique(V(graph)$thr))
    at <- sort(unique(graph$layout[,1]))
    axis(1, at=at, labels=la)
    axis(3, at=at, labels=la)
    par(xpd=FALSE)
    abline(v=at, col="lightgrey", lty=2)
    par(xpd=TRUE)
  }

  tmp <- plot(graph, asp=asp, rescale=rescale,
              xlim=xlim, ylim=ylim, add=TRUE,
              vertex.label=vertex.label, edge.label=edge.label,
              vertex.label.cex=vertex.label.cex, ...)

  if (any(!is.na(vertex.label.topleft))) {
    text(graph$layout[,1]-V(graph)$size/200, graph$layout[,2], pos=3,
         vertex.label.topleft, cex=vertex.label.cex)
  }
  if (any(!is.na(vertex.label.topright))) {
    text(graph$layout[,1]+V(graph)$size/200, graph$layout[,2], pos=3,
         vertex.label.topright, cex=vertex.label.cex)
  }
  if (any(!is.na(vertex.label.bottomleft))) {
    text(graph$layout[,1]-V(graph)$size/200, graph$layout[,2], pos=1,
         vertex.label.bottomleft, cex=vertex.label.cex)
  }
  if (any(!is.na(vertex.label.bottomright))) {
    text(graph$layout[,1]+V(graph)$size/200, graph$layout[,2], pos=1,
         vertex.label.bottomright, cex=vertex.label.cex)
  }

  invisible(tmp)
}
  
ISARobustness <- function(data, isaresult) {

  isa2:::isa.status("Calculating robustness for ExpressionSet", "in")

  data <- eisa.get.nm(data, isaresult)

  if (!is(isaresult, "ISAModules")) {
    stop("Second argument should be the ISA modules")
  }

  nm <- list(Er=t(featExprs(data)),
             Ec=sampExprs(data))

  res <- isa2::robustness(nm, isaresult@genes, isaresult@conditions)

  isa2:::isa.status("DONE", "out")

  res
}

setMethod("isa.filter.robust", signature(data="ExpressionSet"),
          function(data, ...) isa.filter.robust(data, ...))

ISAFilterRobust <- function(data,
                              isaresult, ...) {

  isa2:::isa.status("Filtering ExpressionSet for robustness", "in")

  data <- eisa.get.nm(data, isaresult)
    
  mat.data <- exprs(data)
  nm <- list(Er=t(featExprs(data)),
             Ec=sampExprs(data))
  res <- ISAModules.to.isa.result(isaresult)

  res <- isa.filter.robust(mat.data, nm, res, ...)

  modules <- isa.result.to.ISAModules(res, data)

  isa2:::isa.status("DONE", "out")

  modules
}
