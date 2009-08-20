
setAs(from="Biclust", to="ISAModules", def=function(from) {
  library(biclust)
  new("ISAModules",
      genes=from@RowxNumber,
      conditions=t(from@NumberxCol),
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
