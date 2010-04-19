setGeneric("OrderEV",
    function(biclusters, initialorder, maxtime, debuglevel) standardGeneric("OrderEV"))

setGeneric("ExportEV",
    function(biclusters, eset, order=OrderEV(biclusters), filename=file.choose(),
             norm=c("feature", "sample", "raw", "x", "y"), cutoff=0.95,
             description=NULL, ...) standardGeneric("ExportEV"))
