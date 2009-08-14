

##################
## categoryToEntrezBuilder
## Create a mapping from the categories to the Entrez ids

setMethod("categoryToEntrezBuilder",
         signature(p="miRNAListHyperGParams"),
         function(p) {
           keep.all <- switch(testDirection(p),
                              over=FALSE,
                              under=TRUE,
                              stop("Bad testDirection slot"))

           genes <- unique(unlist(geneIds(p)))
           
           ann <- p@annotation
           org <- get(paste(sep="", ann, "ORGANISM"))
           short.org <- abbreviate(org, 2)
           if (org %in% c("Homo sapiens", "Mus musculus")) {
             miRNA <- toTable(get(paste(sep="", "targetscan.",
                                        short.org, ".egTARGETS")))
           } else {
             stop("Unknown organism in miRNA enrichment")
           }
           
           cat.eg <- tapply(as.character(miRNA[,1]),
                            as.character(miRNA[,2]),
                            c)
           valid <- sapply(cat.eg, function(x) any(genes %in% x))           
           res <- cat.eg[valid]
           res
         })

######################
## universeBuilder
## It returns the Entrez ids from the supplied universe that
## have at least one miRNA annotation
setMethod("universeBuilder", signature=(p="miRNAListHyperGParams"),
          function(p) {
            entrezIds <- universeGeneIds(p)

            ann <- p@annotation
            org <- get(paste(sep="", ann, "ORGANISM"))
            short.org <- abbreviate(org, 2)
            if (org %in% c("Homo sapiens", "Mus musculus")) {
              miRNA <- toTable(get(paste(sep="", "targetscan.",
                                         short.org, ".egTARGETS")))
            } else {
              stop("Unknown organism in miRNA enrichment")
            }

            entrez <- unique(miRNA[,1])
            entrezIds[ entrezIds %in% entrez ]
          })

isa.miRNAListHyperGTest <- function(p) {
  p <- makeValidParams(p)
  
  ## Filter the universe to the genes that have at least one
  ## annotation 
  p@universeGeneIds <- universeBuilder(p)

  ## We need the reverse mapping, the Entrez ids for all miRNA
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

  new("miRNAListHyperGResult",
      reslist=result,
      annotation=p@annotation,
      geneIds=p@geneIds,
      testName=c("miRNA", "List"),
      testDirection=p@testDirection,
      pvalueCutoff=p@pvalueCutoff,
      drive=p@drive,
      universeGeneIds=p@universeGeneIds,
      catToGeneId=dbdcat.ent)
}

#####################
## hyperGTest

setMethod("hyperGTest",
          signature(p="miRNAListHyperGParams"), isa.miRNAListHyperGTest)

ISA.miRNA <- function(modules,
                      org=getOrganism(modules),
                      shortorg=abbreviate(org, 2),
                      ann=annotation(modules),
                      features=featureNames(modules),
                      hgCutoff=0.001,
                      correction=TRUE, correction.method="holm") {

  isa2:::isa.status("Calculating miRNA enrichment", "in")

  if (! org %in% c("Mus musculus", "Homo sapiens")) {
    stop("This method is only implemented for `Mus musculus' and 'Homo sapiens'")
  }

  short.organism <- abbreviate(org, 2)
  require(paste(sep=".", "targetscan", short.organism, "eg.db"),
                character.only=TRUE)
  require(paste(sep="", ann, ".db"), character.only=TRUE)
  require(Category)

  ENTREZ <- get(paste(sep="", ann, "ENTREZID"))

  cat(" -- Extracting Entrez genes\n")

  selectedEntrezIds <- getFeatureNames(modules)
  selectedEntrezIds <- lapply(selectedEntrezIds,
                              function(x) unlist(mget(x, ENTREZ)))
  selectedEntrezIds <- lapply(selectedEntrezIds, unique)

  cat(" -- Extracting Entrez Universe\n")
  entrezUniverse <- unique(unlist(mget(features, ENTREZ)))

  params <-
    try( new("miRNAListHyperGParams", geneIds = selectedEntrezIds,
             universeGeneIds = entrezUniverse, annotation = ann,
             pvalueCutoff = hgCutoff, testDirection = "over", drive=TRUE ))

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

##########################################

convert.miRNA <- function(file) {
  
  tab <- read.delim(file, header=TRUE, comment.char="#")
  tab <- tab[ tab$Species.ID=="9606", ]  # mouse: 10090
  tab <- tab[,2:1]
  tab <- unique(tab)
  tab <- as.matrix(tab)
  tab[] <- sub("^[ ]+", "", tab)
  tab[] <- sub("[ ]+$", "", tab)

  tab
}
    
