
eisa <- function(exp.set) {

  if (!is(exp.set, "ExpressionSet")) {
    stop("Please supply an ExpressionSet object")
  }

  isares <- isa(exprs(exp.set))

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
