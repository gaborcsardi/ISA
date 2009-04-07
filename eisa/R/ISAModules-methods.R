
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
