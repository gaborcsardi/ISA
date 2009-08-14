
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
           require(paste(sep="", "org.", org, ".eg.db"), character.only=TRUE)
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
            require(paste(sep="", "org.", org, ".eg.db"), character.only=TRUE)
            CHR <- get(paste(sep="", "org.", org, ".egCHR"))
            CHR <- as.matrix(toTable(CHR))
            entrez <- unique(CHR[,1])
            entrezIds[ entrezIds %in% entrez ]
          })

isa.CHRListHyperGTest <- function(p) {
  p <- makeValidParams(p)
  
  ## Filter the universe to the genes that have at least one
  ## annotation 
  p@universeGeneIds <- universeBuilder(p)

  ## We need the reverse mapping, the Entrez ids for all CHR
  ## categories (in this subtree).
  dbdcat.ent <- as.list(categoryToEntrezBuilder(p))
  dbdcat.ent <- lapply(dbdcat.ent, intersect, p@universeGeneIds)  

  ## Keep only genes that are in the universe
  p@geneIds <- lapply(p@geneIds, intersect, p@universeGeneIds)

  result <- lapply(p@geneIds, function(genes) {
    count <- sapply(dbdcat.ent, function(x) sum(genes %in% x))
    my.dbdcat.ent <- dbdcat.ent[ count != 0 ]
    count <- count[ count != 0 ]
    size <- sapply(my.dbdcat.ent, length)
    res <- .doHyperGTest(p, my.dbdcat.ent, list(), genes)
    res <- data.frame(Pvalue=res$p, OddsRatio=res$odds,
                      ExpCount=res$expected, Count=count,
                      Size=size, row.names=names(res$p))
    if (p@drive) {
      drive <- lapply(my.dbdcat.ent, intersect, genes)
      drive <- lapply(drive, paste, collapse=";")
      res$drive <- drive
    }
    res[ order(res$Pvalue), ]
  })

  new("CHRListHyperGResult",
      reslist=result,
      annotation=p@annotation,
      geneIds=p@geneIds,
      testName=c("CHR", "List"),
      testDirection=p@testDirection,
      pvalueCutoff=p@pvalueCutoff,
      drive=p@drive,
      universeGeneIds=p@universeGeneIds,
      catToGeneId=dbdcat.ent)
}

#####################
## hyperGTest

setMethod("hyperGTest",
          signature(p="CHRListHyperGParams"), isa.CHRListHyperGTest)

ISA.CHR <- function(modules,
                    org=getOrganism(modules),
                    shortorg=abbreviate(org,2),
                    ann=annotation(modules),
                    features=featureNames(modules),
                    hgCutoff=0.001,
                    correction=TRUE, correction.method="holm") {

  isa2:::isa.status("Calculating chromosome enrichment", "in")
  
  require(paste(sep="", ann, ".db"), character.only=TRUE)
  require(paste(sep=".", "org", shortorg, "eg", "db"), character.only=TRUE)

  ENTREZ <- get(paste(sep="", ann, "ENTREZID"))

  cat(" -- Extracting Entrez genes\n")

  selectedEntrezIds <- getFeatureNames(modules)
  selectedEntrezIds <- lapply(selectedEntrezIds,
                              function(x) unlist(mget(x, ENTREZ)))
  selectedEntrezIds <- lapply(selectedEntrezIds, unique)

  cat(" -- Extracting Entrez Universe\n")
  entrezUniverse <- unique(unlist(mget(features, ENTREZ)))

  params <-
    try( new("CHRListHyperGParams", geneIds = selectedEntrezIds,
             universeGeneIds = entrezUniverse, annotation = ann,
             pvalueCutoff = hgCutoff, testDirection = "over", drive=TRUE ) )

  cat(" -- Doing test\n")
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
