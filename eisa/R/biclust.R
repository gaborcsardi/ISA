
if (require(biclust, quietly=TRUE)) {

  setAs(from="Biclust", to="ISAModules", def=function(from) {

    feat <- rownames(from@Parameters$Data)
    samp <- colnames(from@Parameters$Data)
    anno <- from@Parameters$annotation
    
    if (is.null(feat)) {
      warning("No feature names in `Biclust' object")
    }
    if (is.null(samp)) {
      warning("No sample names in `Biclust' object")
    }
    if (is.null(anno)) {
      warning("No `annotation' in `Biclust' object")
    }
    if (is.null(from@Parameters$pData) && !is.null(samp)) {
      from@Parameters$pData <- data.frame(row.names=samp)
    }
    
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

}

