
eisa <- function(exp.set, flist=filterfun(function(x) IQR(x) > 0.5),
                 thr.gene=seq(2,4,by=0.5), thr.cond=seq(1,3,by=0.5),
                 no.seeds=100) {
  
  if (!is(exp.set, "ExpressionSet")) {
    stop("Please supply an ExpressionSet object")
  }

  if (!is.function(flist) && !is.na(flist)) {
    require(genefilter)
    selected <- genefilter(exp.set, flist)
    exp.set <- exp.set[selected,]
  }
  
  isares <- isa(exprs(exp.set), thr.row=thr.gene, thr.col=thr.cond,
                no.seeds=no.seeds)

  isares <- list(genes=isares$rows, conditions=isares$columns,
                 rundata=isares$rundata, seeddata=isares$seeddata)

  isares$rundata$annotation <- annotation(exp.set)
  library(paste(sep="", annotation(exp.set), ".db"), character.only=TRUE)
  isares$rundata$organism <- get(paste(sep="", annotation(exp.set),
                                       "ORGANISM"))

  isares$rundata$features <- featureNames(exp.set)
  isares$rundata$pData <- pData(exp.set)

  isares
}
