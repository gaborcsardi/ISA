##################
## ontology

setMethod("ontology", "GOListHyperGParams", function(object) object@ontology)

##################
## ontology<-

setReplaceMethod("ontology", c("GOListHyperGParams", "character"),
                 function(r, value) {
                     if (is.na(value) || length(value) != 1)
                       stop("value must be a length one character vector")
                     r@ontology <- value
                     r
                 })

##################
## conditional

setMethod("conditional", "GOListHyperGParams", function(r) r@conditional)

##################
## conditional<-

setReplaceMethod("conditional", c("GOListHyperGParams", "logical"),
                 function(r, value) {
                     if (is.na(value))
                       stop("value must be TRUE or FALSE")
                     r@conditional <- value
                     r
                 })

##################
## categoryToEntrezBuilder
## Create a mapping from the categories to the Entrez ids

setMethod("categoryToEntrezBuilder",
         signature(p="GOListHyperGParams"),
         function(p) {
           keep.all <- switch(testDirection(p),
                              over=FALSE,
                              under=TRUE,
                              stop("Bad testDirection slot"))

           db <- do.call(paste(p@annotation, sep="_", "dbconn"), list())
           univ <- unlist(universeGeneIds(p), use.names=FALSE)

           ## For over representation:
           ## Obtain all unique GO IDs from specified ontology that have at
           ## least one of the genes from geneIds(p) annotated at it.
           ##
           ## For under representation:
           ## Obtain all unique GO IDs from specified ontology that have at
           ## least one of the genes from univ annotated at it.
           ##
           ## These are the GO IDs that form the keys in our GO_to_Entrez map.
           ## First we need to handle the fact that different species have different
           ## mappings for their names.
           if( is(p@datPkg, "YeastDatPkg") || is(p@datPkg, "Org.Sc.sgdDatPkg") ) {
             TABLENAME = "sgd"; GENEIDS="systematic_name"
           } else {
             TABLENAME = "genes"; GENEIDS="gene_id"
           }
           SQL <- "SELECT DISTINCT go_id
FROM %s INNER JOIN go_%s_all USING (_id)
WHERE %s IN (%s)"
           inClause1 <- if (!keep.all)
             unique(unlist(geneIds(p)))
           else
             univ
           inClause1 <- toSQLStringSet(inClause1) # may get reused below
           SQL <- sprintf(SQL, TABLENAME, ontology(p), GENEIDS, inClause1)
           wantedGO <- dbGetQuery(db, SQL)[[1]]
           ## Now collect the Entrez IDs annotated at our wantedGO IDs making
           ## sure to only keep those that are in the gene ID universe
           ## specified in p.
           SQL <- "SELECT DISTINCT %s, go_id
FROM %s INNER JOIN go_%s_all USING (_id)
WHERE %s IN (%s) AND go_id IN (%s)"
           inClauseGO <- toSQLStringSet(wantedGO)
           if (!keep.all)                      # avoid recomputing
             inClause1 <- toSQLStringSet(univ)
           SQL <- sprintf(SQL, GENEIDS, TABLENAME, ontology(p), GENEIDS, inClause1, 
                          inClauseGO)
           ans <- dbGetQuery(db, SQL)
           if (nrow(ans) == 0)
             list()
           else 
             split(ans[[GENEIDS]], ans[["go_id"]])
         })

######################
## universeBuilder
## It returns the Entrez ids from the supplied universe that
## have at least one annotation in the supplied GO ontology
setMethod("universeBuilder", signature=(p="GOListHyperGParams"),
          function(p) {

            datPkg <- p@datPkg
            ontology <- ontology(p)
            entrezIds <- universeGeneIds(p)
            ## Return all Entrez Gene Ids that are annotated at one or more
            ## GO terms belonging to the specified GO ontology.
            ## If 'entrezIds' is given, return the intersection of 'entrezIds'
            ## and the normal return value.
            ontology <- match.arg(ontology, c("BP", "CC", "MF"))
            
            db <- do.call(paste(p@annotation, sep="_", "dbconn"), list())
            if( is(p@datPkg, "YeastDatPkg") || is(p@datPkg, "Org.Sc.sgdDatPkg") ) {
              TABLENAME = "sgd"; GENEIDS="systematic_name"
            } else {
              TABLENAME = "genes"; GENEIDS="gene_id"
            }
            CATTABLE <- paste(sep="", "go_", tolower(ontology))
            
            SQL <- paste(sep="", "SELECT DISTINCT ", GENEIDS,
                         " FROM ", CATTABLE, ",", TABLENAME,
                         " WHERE ", CATTABLE, "._id==", TABLENAME, "._id")
            
            probes <- dbGetQuery(db, SQL)
            probes <- as.character(probes[,1])
            if (length(entrezIds) != 0) {
              probes <- intersect(probes, entrezIds)
            }
            probes
          })

#####################
## hyperGTest

setMethod("hyperGTest",
          signature(p="GOListHyperGParams"), function(p) {
            res <- isa.ListHyperGTest(p)
            res <- do.call(new, c("GOListHyperGResult", res))
            res@testName <- c("GO", "List", ontology(p))
            res@conditional <- conditional(p)
            res
          })

setMethod("htmlReport", signature(r="GOListHyperGResult"),
          function(r, file="", append=FALSE, label="", digits=3, summary.args=NULL) {
            library(xtable)
            library(GO.db)
            summ <- do.call("summary", c(list(r), summary.args))
            for (i in seq_along(summ)) {
              summ[[i]]$Term <- sapply(mget(rownames(summ[[i]]), GOTERM), Term)
            }
            res <- lapply(summ, html.df, label=label, digits=digits,
                          display=c("s", "g", "g", "g", "g", "g", "g", "s"))
            if (!is.null(file)) {
              do.call("cat", c(res, list(file=file, sep="\n\n", append=append)))
              invisible(res)
            } else {
              res
            }
          })  

ISAGO <- function(modules,
                   ann=annotation(modules),
                   features=featureNames(modules),
                   hgCutoff=0.05,
                   correction=TRUE, correction.method="holm") {

  isa2:::isa.status("Calculating GO enrichment", "in")
  
  library(paste(sep="", ann, ".db"), character.only=TRUE)
  
  ENTREZ <- get(paste(sep="", ann, "ENTREZID"))

  selectedEntrezIds <- getFeatureNames(modules)
  selectedEntrezIds <- lapply(selectedEntrezIds,
                              function(x) unlist(mget(x, ENTREZ)))
  selectedEntrezIds <- lapply(selectedEntrezIds, unique)

  entrezUniverse <- unique(unlist(mget(features, ENTREZ)))
  
  paramsBP <- paramsCC <- paramsMF <-
    new("GOListHyperGParams", geneIds = selectedEntrezIds,
        universeGeneIds = entrezUniverse, annotation = ann,
        ontology = "BP", pvalueCutoff = hgCutoff, conditional = FALSE,
        testDirection = "over", drive = TRUE)

  ontology(paramsCC) <- "CC"
  ontology(paramsMF) <- "MF"
  
  hgOverBP <- hyperGTest(paramsBP)
  hgOverCC <- hyperGTest(paramsCC)
  hgOverMF <- hyperGTest(paramsMF)

  res <- list(BP=hgOverBP, CC=hgOverCC, MF=hgOverMF)

  if (correction) {
    for (j in seq_along(res)) {
      for (i in seq_along(res[[j]]@reslist)) {
        res[[j]]@reslist[[i]]$Pvalue <-
          p.adjust(res[[j]]@reslist[[i]]$Pvalue,
                   method=correction.method)
      }
    }
  }
  
  isa2:::isa.status("DONE", "out")

  res
}
