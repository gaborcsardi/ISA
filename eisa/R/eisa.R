
setMethod("isa", signature(data="ExpressionSet"),
          function(data, ...) isa.ExpressionSet(data, ...))

isa.ExpressionSet <- function(data,
                              flist=filterfun(function(x) IQR(x) > 0.5),
                              uniqueEntrez=TRUE,
                              thr.gene=seq(2,4,by=0.5), thr.cond=seq(1,3,by=0.5),
                              no.seeds=100) {

  isa:::isa.status("ISA on an ExpressionSet", "in")
  
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
  
  isares <- isa::isa(exprs(data), thr.row=thr.gene, thr.col=thr.cond,
                     no.seeds=no.seeds)

  isares <- list(genes=isares$rows, conditions=isares$columns,
                 rundata=isares$rundata, seeddata=isares$seeddata)

  isares$rundata$annotation <- annotation(data)
  library(paste(sep="", annotation(data), ".db"), character.only=TRUE)
  isares$rundata$organism <- get(paste(sep="", annotation(data),
                                       "ORGANISM"))

  isares$rundata$features <- featureNames(data)
  isares$rundata$pData <- pData(data)

  isa:::isa.status("DONE", "out")
  
  isares
}
