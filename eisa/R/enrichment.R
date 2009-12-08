
##################################################
## ListHyperGParams
##################################################

##################
## makeValidParams

setMethod("makeValidParams", "ListHyperGParams",
          function(object) {
            ## TODO: proper checks
            if (!is.list(object@geneIds)) {
              object@geneIds <- list(object@geneIds)
            }
            if (object@testDirection != "over") {
              stop("Only overrepresentation test is implemented yet")
            }
            object
          })

##################
## drive

setMethod("drive", signature("ListHyperGParams"), function(p) { p@drive })
setReplaceMethod("drive", c("ListHyperGParams", "logical"),
                 function(p, value) {
                     if (is.na(value))
                       stop("value must be TRUE or FALSE")
                     p@drive <- value
                     p
                 })

##################
## show

setMethod("show", signature(object="ListHyperGParams"),
          function(object) {
              cat("A", class(object), "instance\n")
              cat("  category:", object@categoryName, "\n")
              cat("annotation:", object@annotation, "\n")
          })

##################################################
## ListHyperGResult
##################################################

setMethod("show", signature(object="ListHyperGResult"),
          function(object) {
            no.sign <- sapply(object@reslist, function(x) {
              sum(x$Pvalue < object@pvalueCutoff[1])
            })
            no.sign <- paste(min(no.sign), sep="-", max(no.sign))
            tested <- sum(sapply(object@reslist, function(x) {
              nrow(x)
            }))

            gs <- range(geneMappedCount(object))
            gs <- paste(gs[1], sep="-", gs[2])
            
            cat(description(object), "\n")
            cat(tested, testName(object), "ids tested ")
            cat("(", no.sign, " have p < ", object@pvalueCutoff[1],
                ")\n", sep="")
            cat("Selected gene set sizes:", gs, "\n")
            cat("     Gene universe size:", universeMappedCount(object), "\n")
            cat("     Annotation package:", annotation(object), "\n")
          })

setMethod("summary", signature(object="ListHyperGResult"),
          function(object, pvalue=pvalueCutoff(object), categorySize=NULL) {

            if (! is.null(categorySize)) {
              lapply(object@reslist, function(x) {
                show <- x$Pvalue < pvalue & x$Size >= categorySize
                x[show,]
              })
            } else {
              lapply(object@reslist, function(x) {
                show <- x$Pvalue < pvalue
                x[show,]
              })
            }
          })

setMethod("htmlReport", signature(r="ListHyperGResult"),
          function(r, file="", append=FALSE, label="", digits=3,
                   summary.args=NULL) {
            library(xtable)
            summ <- do.call("summary", c(list(r), summary.args))
            res <- lapply(summ, html.df, label=label, digits=digits,
                          display=c("s", "g", "g", "g", "g", "g", "g"))
            if (!is.null(file)) {
              do.call("cat", c(res, list(file=file, sep="\n\n", append=append)))
              invisible(res)
            } else {
              res
            }
          })

setMethod("pvalues", signature(r="ListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$Pvalue, names=rownames(x))
          }))

setMethod("geneCounts", signature(r="ListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$Count, names=rownames(x))
          }))

setMethod("oddsRatios", signature(r="ListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$OddsRatio, names=rownames(x))
          }))

setMethod("expectedCounts", signature(r="ListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$ExpCount, names=rownames(x))
          }))

setMethod("universeCounts", signature(r="ListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$Size, names=rownames(x))
          }))

setMethod("universeMappedCount", signature(r="ListHyperGResult"),
          function(r) length(r@universeGeneIds))

setMethod("geneMappedCount", signature(r="ListHyperGResult"),
          function(r) sapply(r@geneIds, length))

setMethod("geneIdUniverse", signature(r="ListHyperGResult"),
          function(r, cond=FALSE) r@catToGeneId)

setMethod("condGeneIdUniverse", signature(r="ListHyperGResult"),
          function(r) geneIdUniverse(r, cond=TRUE))

## This function gives all the hits for the tested categories.
setMethod("geneIdsByCategory", signature(r="ListHyperGResult"),
          function(r, catids=NULL) {
            lapply(seq_along(r@geneIds), function(x) {
              if ("drive" %in% names(r@reslist[[x]])) {
                if (is.null(catids)) {
                  drive <- unlist(r@reslist[[x]]$drive)
                } else {
                  drive <- unlist(r@reslist[[x]]$drive[catids])
                }
                strsplit(as.character(drive), ";")
              } else {
                genes <- r@geneIds[[x]]
                if (is.null(catids)) {
                  tmp <- lapply(r@catToGeneId, function(y) {
                    genes [ genes %in% y ]
                  })
                  tmp <- tmp[ sapply(tmp, length) != 0 ]
                  ord <- order(names(tmp))
                  tmp[ord]
                } else {
                  tmp <- lapply(r@catToGeneId[catids], function(y) {
                    genes [ genes %in% y ]
                  })
                }
              }
            })
          })

setMethod("sigCategories", signature(r="ListHyperGResult"),
          function(r, p) {
            if (missing(p)) { p <- pvalueCutoff(r) }
            lapply(r@reslist, function(x) {
              rownames(x)[x$Pvalue < p]
            })
          })

#####################################
## Common function to do the enrichment

isa.ListHyperGTest <- function(p) {
  p <- makeValidParams(p)
  
  ## Filter the universe to the genes that have at least one
  ## annotation 
  p@universeGeneIds <- universeBuilder(p)

  ## We need the reverse mapping, the Entrez ids for all 
  ## categories (in this subtree).
  cat.ent <- as.list(categoryToEntrezBuilder(p))
  cat.ent <- lapply(cat.ent, intersect, p@universeGeneIds)  

  ## Keep only genes that are in the universe
  p@geneIds <- lapply(p@geneIds, intersect, p@universeGeneIds)

  result <- lapply(p@geneIds, function(genes) {
    count <- sapply(cat.ent, function(x) sum(genes %in% x))
    my.cat.ent <- cat.ent[ count != 0 ]
    count <- count[ count != 0 ]
    size <- sapply(my.cat.ent, length)
    res <- .doHyperGTest(p, my.cat.ent, list(), genes)
    res <- data.frame(Pvalue=res$p, OddsRatio=res$odds,
                      ExpCount=res$expected, Count=count,
                      Size=size, row.names=names(res$p))
    if (p@drive) {
      drive <- lapply(my.cat.ent, intersect, genes)
      drive <- lapply(drive, paste, collapse=";")
      res$drive <- drive
    }
    
    res[ order(res$Pvalue), ]
  })

  list(reslist=result,
       annotation=p@annotation,
       geneIds=p@geneIds,
       testDirection=p@testDirection,
       pvalueCutoff=p@pvalueCutoff,
       drive=p@drive,
       universeGeneIds=p@universeGeneIds,
       catToGeneId=cat.ent)
}
 
