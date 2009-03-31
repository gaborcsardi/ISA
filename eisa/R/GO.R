
##################################################
## GOListHyperGParams
##################################################

setClass("GOListHyperGParams",
         representation(ontology="character",
                        conditional="logical",
                        drive="logical"),
         contains="HyperGParams",
         prototype=prototype(categoryName=c("GO", "List"),
           conditional=FALSE, drive=FALSE))

##################
## makeValidParams

setMethod("makeValidParams", "GOListHyperGParams",
          function(object) {
            ## TODO: proper checks
            if (!is.list(object@geneIds)) {
              object@geneIds <- list(object@geneIds)
            }
            if (object@conditional) {
              stop("Conditional GO test is not implemented yet")
            }
            if (object@testDirection != "over") {
              stop("Only overrepresentation test is implemented yet")
            }
            object
          })

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
## drive

setMethod("drive", signature("GOListHyperGParams"), function(r) { r@drive })

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
## drive<-

setReplaceMethod("drive", c("GOListHyperGParams", "logical"),
                 function(r, value) {
                     if (is.na(value))
                       stop("value must be TRUE or FALSE")
                     r@drive <- value
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

isa.GOListHyperGTest <- function(p) {
  p <- makeValidParams(p)
  
  ## Filter the universe to the genes that have at least one
  ## annotation 
  p@universeGeneIds <- universeBuilder(p)

  ## We need the reverse mapping, the Entrez ids for all GO
  ## categories (in this subtree).
  gocat.ent <- as.list(categoryToEntrezBuilder(p))
  gocat.ent <- lapply(gocat.ent, intersect, p@universeGeneIds)  

  ## Keep only genes that are in the universe
  p@geneIds <- lapply(p@geneIds, intersect, p@universeGeneIds)

  result <- lapply(p@geneIds, function(genes) {
    count <- sapply(gocat.ent, function(x) sum(genes %in% x))
    my.gocat.ent <- gocat.ent[ count != 0 ]
    count <- count[ count != 0 ]
    size <- sapply(my.gocat.ent, length)
    res <- .doHyperGTest(p, my.gocat.ent, list(), genes)
    res <- data.frame(Pvalue=res$p, OddsRatio=res$odds,
                      ExpCount=res$expected, Count=count,
                      Size=size, row.names=names(res$p))
    if (p@drive) {
      drive <- lapply(my.gocat.ent, intersect, genes)
      drive <- lapply(drive, paste, collapse=";")
      res$drive <- drive
    }
    res[ order(res$Pvalue), ]
  })

  new("GOListHyperGResult",
      reslist=result,
      annotation=p@annotation,
      geneIds=p@geneIds,
      testName=c("GO", "List", ontology(p)),
      testDirection=p@testDirection,
      pvalueCutoff=p@pvalueCutoff,
      conditional=p@conditional,
      drive=p@drive,
      universeGeneIds=p@universeGeneIds,
      catToGeneId=gocat.ent)
}

#####################
## hyperGTest

setMethod("hyperGTest",
          signature(p="GOListHyperGParams"), isa.GOListHyperGTest)

setMethod("show", signature(object="GOListHyperGParams"),
          function(object) {
              cat("A", class(object), "instance\n")
              cat("  category:", object@categoryName, "\n")
              cat("  ontology:", object@ontology, "\n")
              cat("annotation:", object@annotation, "\n")
          })

##################################################
## GOListHyperGResult
##################################################

setClass("GOListHyperGResult",
         contains="HyperGResultBase",
         representation=representation(
           reslist="list",
           conditional="logical",
           drive="logical",
           universeGeneIds="character",
           catToGeneId="list"),
         prototype=prototype(
           testName="GO",
           reslist=list(),
           universeGeneIds=character(),
           catToGeneId=list()))

setMethod("show", signature(object="GOListHyperGResult"),
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

setMethod("summary", signature(object="GOListHyperGResult"),
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

setMethod("htmlReport", signature=(r="GOListHyperGResult"),
          function(r, file="", append=FALSE, label="", digits=3,
                   summary.args=NULL) {
            callNextMethod(r=r, file=file, append=append,
                           label=label, digits=digits,
                           summary.args=summary.args)            
          })

setMethod("pvalues", signature(r="GOListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$Pvalue, names=rownames(x))
          }))

setMethod("geneCounts", signature(r="GOListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$Count, names=rownames(x))
          }))

setMethod("oddsRatios", signature(r="GOListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$OddsRatio, names=rownames(x))
          }))

setMethod("expectedCounts", signature(r="GOListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$ExpCount, names=rownames(x))
          }))

setMethod("universeCounts", signature(r="GOListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$Size, names=rownames(x))
          }))

setMethod("universeMappedCount", signature(r="GOListHyperGResult"),
          function(r) length(r@universeGeneIds))

setMethod("geneMappedCount", signature(r="GOListHyperGResult"),
          function(r) sapply(r@geneIds, length))

setMethod("geneIdUniverse", signature(r="GOListHyperGResult"),
          function(r, cond=FALSE) r@catToGeneId)

setMethod("condGeneIdUniverse", signature(r="GOListHyperGResult"),
          function(r) geneIdUniverse(r, cond=TRUE))

## This function gives all the hits for the tested categories.
setMethod("geneIdsByCategory", signature(r="GOListHyperGResult"),
          function(r, catids=NULL) {
            lapply(seq_along(r@geneIds), function(x) {
              if ("drive" %in% names(r@reslist[[x]])) {
                drive <- as.character(r@reslist[[x]]$drive)
                strsplit(drive, ";")
              } else {
                genes <- r@geneIds[[x]]
                tmp <- lapply(r@catToGeneId, function(y) {
                  genes [ genes %in% y ]
                })
                tmp <- tmp[ sapply(tmp, length) != 0 ]
                ord <- order(names(tmp))
                tmp[ord]
              }
            })
          })

setMethod("sigCategories", signature(r="GOListHyperGResult"),
          function(r, p) {
            if (missing(p)) { p <- pvalueCutoff(r) }
            lapply(r@reslist, function(x) {
              rownames(x)[x$Pvalue < p]
            })
          })

isa.GO <- function(isaresult, organism=NULL, annotation=NULL, features=NULL,
                   hgCutoff=0.001, correction=TRUE) {

  isa.status("Calculating GO enrichment", "in")
  
  if (is.null(organism)) organism <- isaresult$rundata$organism
  if (is.null(annotation)) annotation <- isaresult$rundata$annotation
  if (is.null(features)) features <- isaresult$rundata$features  
  shortorganism <- abbreviate(organism, 2)
  
  require(paste(sep="", annotation, ".db"), character.only=TRUE)
  require(GO.db)
  require(annotate)
  require(GOstats)
  require(paste(sep=".", "org", shortorganism, "eg", "db"), character.only=TRUE)
  
  ENTREZ <- get(paste(sep="", annotation, "ENTREZID"))

  cat(" -- Extracting Entrez genes\n")

  selectedEntrezIds <- lapply(seq_len(ncol(isaresult$genes)),
                              function(x) features[isaresult$genes[,x] != 0])
  selectedEntrezIds <- lapply(selectedEntrezIds,
                              function(x) unlist(mget(x, ENTREZ)))
  selectedEntrezIds <- lapply(selectedEntrezIds, unique)

  cat(" -- Extracting Entrez Universe\n")
  entrezUniverse <- unique(unlist(mget(features, ENTREZ)))
  
  paramsBP <- paramsCC <- paramsMF <-
    new("GOListHyperGParams", geneIds = selectedEntrezIds,
        universeGeneIds = entrezUniverse, annotation = annotation,
        ontology = "BP", pvalueCutoff = hgCutoff, conditional = FALSE,
        testDirection = "over", drive = TRUE)

  ontology(paramsCC) <- "CC"
  ontology(paramsMF) <- "MF"
  
  hgOverBP <- hgOverCC <- hgOverMF <-
    vector(mode="list", length=length(selectedEntrezIds))

  cat(" -- Doing BP test\n")
  hgOverBP <- hyperGTest(paramsBP)
  cat(" -- Doing CC test\n")
  hgOverCC <- hyperGTest(paramsCC)
  cat(" -- Doing MF test\n")
  hgOverMF <- hyperGTest(paramsMF)
  
  res <- list(hgOverBP, hgOverCC, hgOverMF)

  isa.status("DONE", "out")

  res
}
