
annotate <- function(biclusters, data) {

  if (!inherits(biclusters, "Biclust")) {
    stop("`biclusters' must be a Biclust")
  }
  
  if (!inherits(data, "ExpressionSet")) {
    stop("`data' must be an ExpressionSet")
  }

  par <- biclusters@Parameters
  par$annotation <- annotation(data)
  par$featureNames <- featureNames(data)
  par$sampleNames <- sampleNames(data)
  par$pData <- pData(data)
  biclusters@Parameters <- par

  biclusters
}

setAs(from="Biclust", to="ISAModules", def=function(from) {

  feat <- from@Parameters$featureNames
  samp <- from@Parameters$sampleNames

  new("ISAModules",
      genes=structure(from@RowxNumber, dimnames=list(feat, NULL)),
      conditions=structure(t(from@NumberxCol), dimnames=list(samp, NULL)),
      rundata=from@Parameters,
      seeddata=data.frame())
})

setAs(from="ISAModules", to="Biclust", def=function(from) {
  library(biclust)
  new("Biclust", Parameters=list(seeddata=from@seeddata,
                   rundata=from@rundata),
      RowxNumber=from@genes != 0,
      NumberxCol=t(from@conditions != 0), Number=ncol(from@genes))
})

