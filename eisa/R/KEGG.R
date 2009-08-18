
##################
## categoryToEntrezBuilder
## Create a mapping from the categories to the Entrez ids

setMethod("categoryToEntrezBuilder",
         signature(p="KEGGListHyperGParams"),
         function(p) {
           keep.all <- switch(testDirection(p),
                              over=FALSE,
                              under=TRUE,
                              stop("Bad testDirection slot"))

           geneIds <- unique(unlist(geneIds(p)))
           lib <- annotation(p)
           isORGEG = grep("org.*.eg", lib)
           if( length(isORGEG) > 0 )
             kegg2allprobes <- Category:::getDataEnv("PATH2EG", lib)
           else
             kegg2allprobes <- Category:::getDataEnv("PATH2PROBE", lib)
           probeAnnot <- Category:::getKeggToProbeMap(kegg2allprobes)
           Category:::probeToEntrezMapHelper(probeAnnot, geneIds, p@datPkg, universeGeneIds(p),
                                             keep.all=keep.all)
         
         })

######################
## universeBuilder
## It returns the Entrez ids from the supplied universe that
## have at least one KEGG annotation
setMethod("universeBuilder", signature=(p="KEGGListHyperGParams"),
          function(p) {
            entrezIds <- universeGeneIds(p)
            SQL <- "select distinct gene_id from genes, kegg where genes._id = kegg._id"
            db <- do.call(paste(p@annotation, sep="_", "dbconn"), list())            
            univ <- dbGetQuery(db, SQL)[[1]]
            if (!is.null(entrezIds) && length(entrezIds) > 0)
              univ <- intersect(univ, unlist(entrezIds))
            if (length(univ) < 1)
              stop("No Entrez Gene ids left in universe")
            univ
          })

#####################
## hyperGTest

setMethod("hyperGTest",
          signature(p="KEGGListHyperGParams"), function(p) {
            res <- isa.ListHyperGTest(p)
            do.call(new, c("KEGGListHyperGResult", res))
          })

ISA.KEGG <- function(modules,
                     ann=annotation(modules),
                     features=featureNames(modules),
                     hgCutoff=0.05,
                     correction=TRUE, correction.method="holm") {

  isa2:::isa.status("Calculating KEGG enrichment", "in")
  
  require(paste(sep="", ann, ".db"), character.only=TRUE)
  require(Category)
  require(KEGG.db)

  ENTREZ <- get(paste(sep="", ann, "ENTREZID"))

  cat(" -- Extracting Entrez genes\n")

  selectedEntrezIds <- getFeatureNames(modules)
  selectedEntrezIds <- lapply(selectedEntrezIds,
                              function(x) unlist(mget(x, ENTREZ)))
  selectedEntrezIds <- lapply(selectedEntrezIds, unique)

  cat(" -- Extracting Entrez Universe\n")
  entrezUniverse <- unique(unlist(mget(features, ENTREZ)))

  params <-
    try( new("KEGGListHyperGParams", geneIds = selectedEntrezIds,
             universeGeneIds = entrezUniverse, annotation = ann,
             pvalueCutoff = hgCutoff, testDirection = "over", drive=TRUE) )

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
