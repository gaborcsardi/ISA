
##################
## categoryToEntrezBuilder
## Create a mapping from the categories to the Entrez ids

setMethod("categoryToEntrezBuilder",
         signature(p="GeneralListHyperGParams"),
         function(p) {
           p@categories
         })

######################
## universeBuilder
## It returns the Entrez ids from the supplied universe that
## have at least one General annotation
setMethod("universeBuilder", signature=(p="GeneralListHyperGParams"),
          function(p) {
            unique(unlist(p@categories))
          })

#####################
## hyperGTest

setMethod("hyperGTest",
          signature(p="GeneralListHyperGParams"), function(p) {
            res <- isa.ListHyperGTest(p)
            do.call(new, c("GeneralListHyperGResult", res))
          })

setMethod("htmlReport", signature(r="GeneralListHyperGResult"),
          function(r, file="", append=FALSE, label="", digits=3, summary.args=NULL) {
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

ISAEnrichment <- function(modules, categories,
                          ann=annotation(modules),
                          features=featureNames(modules),
                          hgCutoff=0.05,
                          correction=TRUE, correction.method="holm") {

  isa2:::isa.status("Calculating KEGG enrichment", "in")

  if (!is.list(categories) || is.null(names(categories))) {
    stop("categories most be a named list")
  }
  
  library(paste(sep="", ann, ".db"), character.only=TRUE)

  ENTREZ <- get(paste(sep="", ann, "ENTREZID"))

  selectedEntrezIds <- getFeatureNames(modules)
  selectedEntrezIds <- lapply(selectedEntrezIds,
                              function(x) unlist(mget(x, ENTREZ)))
  selectedEntrezIds <- lapply(selectedEntrezIds, unique)

  entrezUniverse <- unique(unlist(mget(features, ENTREZ)))

  params <-
    try( new("GeneralListHyperGParams", geneIds = selectedEntrezIds,
             universeGeneIds = entrezUniverse, annotation = ann,
             pvalueCutoff = hgCutoff, testDirection = "over", drive=TRUE,
             categories=categories) )

  hgOver <- hyperGTest(params)

  if (correction) {
    for (i in seq_along(hgOver@reslist)) {
      hgOver@reslist[[i]]$Pvalue <- p.adjust(hgOver@reslist[[i]]$Pvalue,
                                             method=correction.method)
    }
  }
  
  isa2:::isa.status("DONE", "out")
  
  hgOver
}
