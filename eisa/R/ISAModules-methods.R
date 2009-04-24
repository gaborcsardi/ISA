
setMethod("show", signature(object="ISAModules"),
          function(object) {
            cat("An ISAModules instance.\n")
            cat("  Number of modules:", ncol(object@genes), "\n")
            cat("  Number of genes:", nrow(object@genes), "\n")
            cat("  Number of conditions:", nrow(object@conditions), "\n")
            cat("  Gene threshold(s):",
                paste(collapse=", ", unique(object@seeddata$thr.row)), "\n")
            cat("  Conditions threshold(s):",
                paste(collapse=", ", unique(object@seeddata$thr.col)), "\n")
          })

setMethod("length", signature(x="ISAModules"),
          function(x) {
            ncol(x@genes)
          })

setMethod("dim", signature(x="ISAModules"),
          function(x) {
            c(nrow(x@genes), nrow(x@conditions))
          })

setMethod("featureNames", signature(object="ISAModules"),
          function(object) {
            object@rundata$features
          })

setMethod("sampleNames", signature(object="ISAModules"),
          function(object) {
            rownames(object@rundata$pData)
          })

setMethod("pData", signature(object="ISAModules"),
          function(object) {
            object@rundata$pData
          })

setMethod("seedData", signature(object="ISAModules"),
          function(object) {
            object@seeddata
          })

setMethod("runData", signature(object="ISAModules"),
          function(object) {
            res <- object@rundata
            res[["pData"]] <- NULL
            res[["features"]] <- NULL
            res
          })

setMethod("annotation", signature(object="ISAModules"),
          function(object) {
            object@rundata$annotation
          })

setMethod("organism", signature(object="ISAModules"),
          function(object) {
            object@rundata$organism
          })

setMethod("getGenes", signature(object="ISAModules"),
          function(object, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(object@genes)) }
            lapply(mods, function(x)
                   featureNames(object)[ object@genes[,x] != 0 ])
          })

setMethod("getConditions", signature(object="ISAModules"),
          function(object, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(object@genes)) }
            lapply(mods, function(x)
                   sampleNames(object)[ object@conditions[,x] != 0 ])
          })

setMethod("getGeneScores", signature(object="ISAModules"),
          function(object, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(object@genes)) }
            lapply(mods, function(x)
                   object@genes[,x][ object@genes[,x] != 0 ])
          })


setMethod("getConditionScores", signature(object="ISAModules"),
          function(object, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(object@genes)) }
            lapply(mods, function(x)
                   object@conditions[,x][ object@conditions[,x] != 0 ])
          })

setMethod("getAllGeneScores", signature(object="ISAModules"),
          function(object, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(object@genes)) }
            lapply(mods, function(x)
                   object@genes[,x,drop=FALSE])
          })


setMethod("getAllConditionScores", signature(object="ISAModules"),
          function(object, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(object@genes)) }
            lapply(mods, function(x)
                   object@conditions[,x,drop=FALSE])
          })

setMethod("getNoGenes", signature(object="ISAModules"),
          function(object, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(object@genes)) }
            colSums(object@genes[,mods,drop=FALSE] != 0)
          })
            
setMethod("getNoConditions", signature(object="ISAModules"),
          function(object, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(object@genes)) }
            colSums(object@conditions[,mods,drop=FALSE] != 0)
          })
            
setMethod("[", signature(x="ISAModules"),
          function(x, i, j, ..., drop=FALSE) {
            if (!missing(j)) {
              x@conditions <- x@conditions[j,,drop=FALSE]
              x@rundata$pData <- x@rundata$pData[j,,drop=FALSE]
            }
            if (!missing(i)) {
              x@genes <- x@genes[i,,drop=FALSE]
              x@rundata$features <- x@rundata$features[i]
            }
            x
          })

setMethod("[[", signature(x="ISAModules"),
          function(x, i, j, ..., drop=FALSE) {
            if (!missing(i)) {
              x@genes <- x@genes[,i,drop=FALSE]
              x@conditions <- x@conditions[,i,drop=FALSE]
              x@seeddata <- x@seeddata[i,]
            }
            x
          })

setMethod("geneThreshold", signature(object="ISAModules"),
          function(object, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(object@genes)) }
            object@seeddata$thr.row[mods]
          })

setMethod("conditionThreshold", signature(object="ISAModules"),
          function(object, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(object@genes)) }
            object@seeddata$thr.col[mods]
          })
