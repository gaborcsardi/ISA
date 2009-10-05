
##################
## categoryToEntrezBuilder
## Create a mapping from the categories to the Entrez ids

setMethod("categoryToEntrezBuilder",
         signature(p="CHRListHyperGParams"),
         function(p) {
           keep.all <- switch(testDirection(p),
                              over=FALSE,
                              under=TRUE,
                              stop("Bad testDirection slot"))

           genes <- unique(unlist(geneIds(p)))

           annotation <- p@annotation
           org <- get(paste(sep="", annotation, "ORGANISM"))
           org <- abbreviate(org, 2)
           library(paste(sep="", "org.", org, ".eg.db"), character.only=TRUE)
           CHR <- get(paste(sep="", "org.", org, ".egCHR"))
           CHR <- as.matrix(toTable(CHR))

           cat.eg <- tapply(as.character(CHR[,1]),
                            as.character(CHR[,2]),
                            c)
           valid <- sapply(cat.eg, function(x) any(genes %in% x))           
           res <- cat.eg[valid]
           res
         })

######################
## universeBuilder
## It returns the Entrez ids from the supplied universe that
## have at least one CHR annotation
setMethod("universeBuilder", signature=(p="CHRListHyperGParams"),
          function(p) {
            entrezIds <- universeGeneIds(p)
            annotation <- p@annotation
            org <- get(paste(sep="", annotation, "ORGANISM"))
            org <- abbreviate(org, 2)
            library(paste(sep="", "org.", org, ".eg.db"), character.only=TRUE)
            CHR <- get(paste(sep="", "org.", org, ".egCHR"))
            CHR <- as.matrix(toTable(CHR))
            entrez <- unique(CHR[,1])
            entrezIds[ entrezIds %in% entrez ]
          })

#####################
## hyperGTest

setMethod("hyperGTest",
          signature(p="CHRListHyperGParams"), function(p) {
            res <- isa.ListHyperGTest(p)
            do.call(new, c("CHRListHyperGResult", res))
          })

ISACHR <- function(modules,
                    ann=annotation(modules),
                    features=featureNames(modules),
                    hgCutoff=0.05,
                    correction=TRUE, correction.method="holm") {

  isa2:::isa.status("Calculating chromosome enrichment", "in")
  
  library(paste(sep="", ann, ".db"), character.only=TRUE)

  ENTREZ <- get(paste(sep="", ann, "ENTREZID"))

  selectedEntrezIds <- getFeatureNames(modules)
  selectedEntrezIds <- lapply(selectedEntrezIds,
                              function(x) unlist(mget(x, ENTREZ)))
  selectedEntrezIds <- lapply(selectedEntrezIds, unique)

  entrezUniverse <- unique(unlist(mget(features, ENTREZ)))

  params <-
    try( new("CHRListHyperGParams", geneIds = selectedEntrezIds,
             universeGeneIds = entrezUniverse, annotation = ann,
             pvalueCutoff = hgCutoff, testDirection = "over", drive=TRUE ) )

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
