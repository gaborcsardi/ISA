
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

  new.modules@rundata$features <- featureNames(data)
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

eisa.get.nm <- function(data) {
  if (is(data, "ISAExpressionSet")) {
    data
  } else {
    ISA.normalize(data)
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
    require(genefilter)
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

ISA.normalize <- function(data, prenormalize=TRUE) {

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

ISA.iterate <- function(data, feature.seeds, sample.seeds,
                        thr.feat, thr.samp=thr.feat, ...) {
  
  isa2:::isa.status("Starting ISA iteration on an ExpressionSet", "in")

  data <- eisa.get.nm(data)
  
  nm <- list(Er=t(feat.exprs(data)),
             Ec=samp.exprs(data))

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

ISA.unique <- function(data, isaresult, ...) {

  isa2:::isa.status("Creating unique ISA module set for expression data", "in")
  
  data <- eisa.get.nm(data)
  
  nm <- list(Er=t(feat.exprs(data)),
             Ec=samp.exprs(data))

  res <- ISAModules.to.isa.result(isaresult)

  res <- isa2::isa.unique(nm, res, ...)

  modules <- isa.result.to.ISAModules(res, data)

  isa2:::isa.status("DONE", "out")
  
  modules
}

ISA.sweep <- function(data, isaresult, ...) {

  isa2:::isa.status("Sweeping an ExpressionSet")

  data <- eisa.get.nm(data)
    
  mat.data <- exprs(data)
  nm <- list(Er=t(feat.exprs(data)),
             Ec=samp.exprs(data))
  res <- ISAModules.to.isa.result(isaresult)

  res <- isa.sweep(mat.data, nm, res, ...)

  modules <- isa.result.to.ISAModules(res, data)

  isa2:::isa.status("DONE", "out")

  modules
}

ISA.sweep.graph <- function(sweep.result) {
  res <- ISAModules.to.isa.result(sweep.result)
  sweep.graph(res)
}

ISA.robustness <- function(data, isaresult) {

  isa2:::isa.status("Calculating robustness for ExpressionSet", "in")

  data <- eisa.get.nm(data)

  if (!is(isaresult, "ISAModules")) {
    stop("Second argument should be the ISA modules")
  }

  nm <- list(Er=t(feat.exprs(data)),
             Ec=samp.exprs(data))

  print(dim(nm[[1]]))
  print(dim(nm[[2]]))        
  res <- isa2::robustness(nm, isaresult@genes, isaresult@conditions)

  isa2:::isa.status("DONE", "out")

  res
}

setMethod("isa.filter.robust", signature(data="ExpressionSet"),
          function(data, ...) isa.filter.robust(data, ...))

ISA.filter.robust <- function(data,
                              isaresult, ...) {

  isa2:::isa.status("Filtering ExpressionSet for robustness", "in")

  data <- eisa.get.nm(data)
    
  mat.data <- exprs(data)
  nm <- list(Er=t(feat.exprs(data)),
             Ec=samp.exprs(data))
  res <- ISAModules.to.isa.result(isaresult)

  res <- isa.filter.robust(mat.data, nm, res, ...)

  modules <- isa.result.to.ISAModules(res, data)

  isa2:::isa.status("DONE", "out")

  modules
}
