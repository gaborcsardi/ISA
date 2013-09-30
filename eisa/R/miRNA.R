

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

#####################
## hyperGTest

setMethod("hyperGTest",
          signature(p="miRNAListHyperGParams"), function(p) {
            res <- isa.ListHyperGTest(p)
            do.call(new, c("miRNAListHyperGResult", res))
          })

ISAmiRNA <- function(modules,
                      ann=annotation(modules),
                      features=featureNames(modules),
                      hgCutoff=0.05,
                      correction=TRUE, correction.method="holm") {

  isa2:::isa.status("Calculating miRNA enrichment", "in")

  org <- getOrganism(modules)
  short.organism <- abbreviate(org, 2)
  library(paste(sep=".", "targetscan", short.organism, "eg.db"),
                character.only=TRUE)
  library(paste(sep="", ann, ".db"), character.only=TRUE)

  ENTREZ <- get(paste(sep="", ann, "ENTREZID"))

  selectedEntrezIds <- getFeatureNames(modules)
  selectedEntrezIds <- lapply(selectedEntrezIds,
                              function(x) unlist(mget(x, ENTREZ)))
  selectedEntrezIds <- lapply(selectedEntrezIds, unique)

  entrezUniverse <- unique(unlist(mget(features, ENTREZ)))

  params <-
    try( new("miRNAListHyperGParams", geneIds = selectedEntrezIds,
             universeGeneIds = entrezUniverse, annotation = ann,
             pvalueCutoff = hgCutoff, testDirection = "over", drive=TRUE ))

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
    
