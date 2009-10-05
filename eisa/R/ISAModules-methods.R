
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

setMethod("seedData", signature(modules="ISAModules"),
          function(modules) {
            modules@seeddata
          })

setMethod("runData", signature(modules="ISAModules"),
          function(modules) {
            res <- modules@rundata
            res[["pData"]] <- NULL
            res
          })

setMethod("annotation", signature(object="ISAModules"),
          function(object) {
            object@rundata$annotation
          })

setMethod("getOrganism", signature(modules="ISAModules"),
          function(modules) {
            if (!is.null(modules@rundata$organism)) {
              modules@rundata$organism
            } else if (!is.null(modules@rundata$annotation)) {
              library(paste(sep="", annotation(modules), ".db"), character.only=TRUE)
              get(paste(sep="", annotation(modules), "ORGANISM"))
            } else {
              NULL
            }
          })

setMethod("getFeatures", signature(modules="ISAModules"),
          function(modules, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(modules@genes)) }
            lapply(mods, function(x) which(modules@genes[,x] != 0))
          })

setMethod("getFeatureNames", signature(modules="ISAModules"),
          function(modules, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(modules@genes)) }
            lapply(mods, function(x)
                   featureNames(modules)[ modules@genes[,x] != 0 ])
          })

setMethod("getSamples", signature(modules="ISAModules"),
          function(modules, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(modules@genes)) }
            lapply(mods, function(x) which(modules@conditions[,x] != 0))
          })

setMethod("getSampleNames", signature(modules="ISAModules"),
          function(modules, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(modules@genes)) }
            lapply(mods, function(x)
                   sampleNames(modules)[ modules@conditions[,x] != 0 ])
          })

setMethod("getFeatureMatrix", signature(modules="ISAModules"),
          function(modules, binary=FALSE, sparse=FALSE, mods) {
            if (missing(mods)) {
              res <- modules@genes
            } else {
              res <- modules@genes[,mods,drop=FALSE]
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

setMethod("getSampleMatrix", signature(modules="ISAModules"),
          function(modules, binary=FALSE, sparse=FALSE, mods) {
            if (missing(mods)) {
              res <- modules@conditions
            } else {
              res <- modules@conditions[,mods,drop=FALSE]
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

setMethod("getFullFeatureMatrix", signature(modules="ISAModules"),
          function(modules, eset, mods) {
            eset <- eisa.get.nm(eset, modules)
            nm2 <- sampExprs(eset)
            if (missing(mods)) {
              genes <- getFeatureMatrix(modules)
              samp  <- getSampleMatrix(modules)
              thr <- featureThreshold(modules)
            } else {
              genes <- getFeatureMatrix(modules, mods=mods)
              samp <- getSampleMatrix(modules, mods=mods)
              thr <- featureThreshold(modules)[mods]
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

setMethod("getFullSampleMatrix", signature(modules="ISAModules"),
          function(modules, eset, mods) {
            eset <- eisa.get.nm(eset, modules)
            nm1 <- t(featExprs(eset))
            if (missing(mods)) {
              genes <- getFeatureMatrix(modules)
              samp  <- getSampleMatrix(modules)
              thr <- sampleThreshold(modules)
            } else {
              genes <- getFeatureMatrix(modules, mods=mods)
              samp <- getSampleMatrix(modules, mods=mods)
              thr <- sampleThreshold(modules)[mods]
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

setMethod("getFeatureScores", signature(modules="ISAModules"),
          function(modules, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(modules@genes)) }
            lapply(mods, function(x)
                   modules@genes[,x][ modules@genes[,x] != 0 ])
          })


setMethod("getSampleScores", signature(modules="ISAModules"),
          function(modules, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(modules@genes)) }
            lapply(mods, function(x)
                   modules@conditions[,x][ modules@conditions[,x] != 0 ])
          })

setMethod("getNoFeatures", signature(modules="ISAModules"),
          function(modules, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(modules@genes)) }
            colSums(modules@genes[,mods,drop=FALSE] != 0)
          })
            
setMethod("getNoSamples", signature(modules="ISAModules"),
          function(modules, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(modules@genes)) }
            colSums(modules@conditions[,mods,drop=FALSE] != 0)
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

setMethod("featureThreshold", signature(modules="ISAModules"),
          function(modules, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(modules@genes)) }
            modules@seeddata$thr.row[mods]
          })

setMethod("sampleThreshold", signature(modules="ISAModules"),
          function(modules, mods) {
            if (missing(mods)) { mods <- seq_len(ncol(modules@genes)) }
            modules@seeddata$thr.col[mods]
          })
