
setMethod("show", signature(object="ISAModules"),
          function(object) {
            cat("An ISAModules instance.\n")
            cat("  Number of modules:", ncol(object@genes), "\n")
            cat("  Number of features:", nrow(object@genes), "\n")
            cat("  Number of samples:", nrow(object@conditions), "\n")
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
            rownames(object@genes)
          })

setMethod("sampleNames", signature(object="ISAModules"),
          function(object) {
            rownames(object@conditions)
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
            res
          })

setMethod("annotation", signature(object="ISAModules"),
          function(object) {
            object@rundata$annotation
          })

setMethod("getOrganism", signature(object="ISAModules"),
          function(object) {
            if (!is.null(object@rundata$organism)) {
              object@rundata$organism
            } else if (!is.null(object@rundata$annotation)) {
              library(paste(sep="", annotation(object), ".db"), character.only=TRUE)
              get(paste(sep="", annotation(object), "ORGANISM"))
            } else {
              NULL
            }
          })

setMethod("getFeatures", signature(object="ISAModules"),
          function(object, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(object@genes)) }
            lapply(mods, function(x) which(object@genes[,x] != 0))
          })

setMethod("getFeatureNames", signature(object="ISAModules"),
          function(object, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(object@genes)) }
            lapply(mods, function(x)
                   featureNames(object)[ object@genes[,x] != 0 ])
          })

setMethod("getSamples", signature(object="ISAModules"),
          function(object, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(object@genes)) }
            lapply(mods, function(x) which(object@conditions[,x] != 0))
          })

setMethod("getSampleNames", signature(object="ISAModules"),
          function(object, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(object@genes)) }
            lapply(mods, function(x)
                   sampleNames(object)[ object@conditions[,x] != 0 ])
          })

setMethod("getFeatureMatrix", signature(object="ISAModules"),
          function(object, binary=FALSE, sparse=FALSE, mods) {
            if (missing(mods)) {
              res <- object@genes
            } else {
              res <- object@genes[,mods,drop=FALSE]
            }
            if (binary) {
              res <- res != 0
            }
            if (sparse) {
              library(Matrix)
              res <- Matrix(res, sparse=TRUE)
            }
            res
          })

setMethod("getSampleMatrix", signature(object="ISAModules"),
          function(object, binary=FALSE, sparse=FALSE, mods) {
            if (missing(mods)) {
              res <- object@conditions
            } else {
              res <- object@conditions[,mods,drop=FALSE]
            }
            if (binary) {
              res <- res != 0
            }
            if (sparse) {
              library(Matrix)
              res <- Matrix(res, sparse=TRUE)
            }
            res
          })            

setMethod("getFullFeatureMatrix", signature(object="ISAModules"),
          function(object, eset, mods) {
            eset <- eisa.get.nm(eset, object)
            nm2 <- samp.exprs(eset)
            if (missing(mods)) {
              genes <- getFeatureMatrix(object)
              samp  <- getSampleMatrix(object)
              thr <- featureThreshold(object)
            } else {
              genes <- getFeatureMatrix(object, mods=mods)
              samp <- getSampleMatrix(object, mods=mods)
              thr <- featureThreshold(object)[mods]
            }
            scores <- nm2 %*% samp
            n.scores <- ifelse(genes != 0, scores, 0)
            for (i in seq_len(ncol(scores))) {
              m <- max(abs(n.scores[,i]))
              if (m!=0) { 
                scores[,i] <- scores[,i] / m                  
              }
            }
            scores            
          })

setMethod("getFullSampleMatrix", signature(object="ISAModules"),
          function(object, eset, mods) {
            eset <- eisa.get.nm(eset, object)
            nm1 <- t(feat.exprs(eset))
            if (missing(mods)) {
              genes <- getFeatureMatrix(object)
              samp  <- getSampleMatrix(object)
              thr <- sampleThreshold(object)
            } else {
              genes <- getFeatureMatrix(object, mods=mods)
              samp <- getSampleMatrix(object, mods=mods)
              thr <- sampleThreshold(object)[mods]
            }
            scores <- nm1 %*% genes
            n.scores <- ifelse(samp != 0, scores, 0)
            for (i in seq_len(ncol(scores))) {
              m <- max(abs(n.scores[,i]))
              if (m!=0) { 
                scores[,i] <- scores[,i] / m                  
              }
            }
            scores
          })

setMethod("getFeatureScores", signature(object="ISAModules"),
          function(object, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(object@genes)) }
            lapply(mods, function(x)
                   object@genes[,x][ object@genes[,x] != 0 ])
          })


setMethod("getSampleScores", signature(object="ISAModules"),
          function(object, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(object@genes)) }
            lapply(mods, function(x)
                   object@conditions[,x][ object@conditions[,x] != 0 ])
          })

setMethod("getNoFeatures", signature(object="ISAModules"),
          function(object, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(object@genes)) }
            colSums(object@genes[,mods,drop=FALSE] != 0)
          })
            
setMethod("getNoSamples", signature(object="ISAModules"),
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

setMethod("featureThreshold", signature(object="ISAModules"),
          function(object, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(object@genes)) }
            object@seeddata$thr.row[mods]
          })

setMethod("sampleThreshold", signature(object="ISAModules"),
          function(object, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(object@genes)) }
            object@seeddata$thr.col[mods]
          })
