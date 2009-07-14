
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

setMethod("isa", signature(data="ExpressionSet"),
          function(data, ...) isa.ExpressionSet(data, ...))

isa.ExpressionSet <- function(data,
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

setMethod("isa.normalize", signature(data="ExpressionSet"),
          function(data, ...) isa.normalize.ExpressionSet(data, ...))

isa.normalize.ExpressionSet <- function(data, prenormalize=FALSE) {

  isa2:::isa.status("Normalizing ExpressionSet", "in")

  data <- as(data, "ISAExpressionSet")
  normed.data.exprs <- isa2::isa.normalize(exprs(data),
                                          prenormalize=prenormalize)

  data@assayData <- assayDataNew(exprs=t(normed.data.exprs$Er),
                                 ec.exprs=normed.data.exprs$Ec)

  attr(data, "prenormalize") <- attr(normed.data.exprs, "prenormalize")
  attr(data, "hasNA") <- attr(normed.data.exprs, "hasNA")

  isa2:::isa.status("DONE", "out")

  data
}

setMethod("isa.iterate", signature(normed.data="ExpressionSet"),
          function(normed.data, ...) isa.iterate.ExpressionSet(normed.data, ...))

isa.iterate.ExpressionSet <- function(normed.data, ...) {

  isa2:::isa.status("Starting ISA iteration on an ExpressionSet", "in")

  nm <- list(Er=t(feat.exprs(normed.data)),
             Ec=samp.exprs(normed.data))

  attr(nm, "prenormalize") <- prenormalized(normed.data)
  attr(nm, "hasNA") <- hasNA(normed.data)

  modules <- isa.iterate(nm, ...)
  modules <- isa.result.to.ISAModules(modules, normed.data)
  
  isa2:::isa.status("DONE", "out")
  
  modules
}

setMethod("isa.unique", signature(normed.data="ExpressionSet",
                                  isaresult="ISAModules"),
          function(normed.data, isaresult, ...)
          isa.unique.ExpressionSet(normed.data, isaresult, ...))

isa.unique.ExpressionSet <- function(normed.data, isaresult, ...) {

  isa2:::isa.status("Creating unique ISA module set for expression data", "in")
  
  nm <- list(Er=t(feat.exprs(normed.data)),
             Ec=samp.exprs(normed.data))

  res <- ISAModules.to.isa.result(isaresult)

  res <- isa2::isa.unique(nm, res, ...)

  modules <- isa.result.to.ISAModules(res, normed.data)

  isa2:::isa.status("DONE", "out")
  
  modules
}

setMethod("isa.sweep", signature(data="ExpressionSet"),
          function(data, ...) isa.sweep.ExpressionSet(data, ...))

isa.sweep.ExpressionSet <- function(data, normed.data, isaresult, ...) {

  isa2:::isa.status("Sweeping an ExpressionSet")

  mat.data <- exprs(data)
  nm <- list(Er=t(feat.exprs(normed.data)),
             Ec=samp.exprs(normed.data))
  res <- ISAModules.to.isa.result(isaresult)

  res <- isa.sweep(mat.data, nm, res, ...)

  modules <- isa.result.to.ISAModules(res, normed.data)

  isa2:::isa.status("DONE", "out")

  modules
}

setMethod("sweep.graph", signature(sweep.result="ISAModules"),
          function(sweep.result, ...)
          sweep.graph.ExpressionSet(sweep.result, ...))

sweep.graph.ExpressionSet <- function(sweep.result) {
  res <- ISAModules.to.isa.result(sweep.result)
  sweep.graph(res)
}

setMethod("robustness", signature(normed.data="ExpressionSet"),
          function(normed.data, ...) robustness.ExpressionSet(normed.data, ...))

robustness.ExpressionSet <- function(normed.data, isaresult) {

  isa2:::isa.status("Calculating robustness for ExpressionSet", "in")

  if (!is(isaresult, "ISAModules")) {
    stop("Second argument should be the ISA modules")
  }

  nm <- list(Er=t(feat.exprs(normed.data)),
             Ec=samp.exprs(normed.data))

  print(dim(nm[[1]]))
  print(dim(nm[[2]]))        
  res <- isa2::robustness(nm, isaresult@genes, isaresult@conditions)

  isa2:::isa.status("DONE", "out")

  res
}

setMethod("isa.filter.robust", signature(data="ExpressionSet"),
          function(data, ...) isa.filter.robust.ExpressionSet(data, ...))

isa.filter.robust.ExpressionSet <- function(data, normed.data,
                                            isaresult, ...) {

  isa2:::isa.status("Filtering ExpressionSet for robustness", "in")

  mat.data <- exprs(data)
  nm <- list(Er=t(feat.exprs(normed.data)),
             Ec=samp.exprs(normed.data))
  res <- ISAModules.to.isa.result(isaresult)

  res <- isa.filter.robust(mat.data, nm, res, ...)

  modules <- isa.result.to.ISAModules(res, normed.data)

  isa2:::isa.status("DONE", "out")

  modules
}
