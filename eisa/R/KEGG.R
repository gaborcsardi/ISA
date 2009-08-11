
##################################################
## KEGGListHyperGParams
##################################################

setClass("KEGGListHyperGParams",
         representation(drive="logical"),
         contains="HyperGParams",
         prototype=prototype(categoryName=c("KEGG", "List"),
           drive=FALSE))

##################
## makeValidParams

setMethod("makeValidParams", "KEGGListHyperGParams",
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

setMethod("drive", signature("KEGGListHyperGParams"), function(r) { r@drive })
setReplaceMethod("drive", c("KEGGListHyperGParams", "logical"),
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

isa.KEGGListHyperGTest <- function(p) {
  p <- makeValidParams(p)
  
  ## Filter the universe to the genes that have at least one
  ## annotation 
  p@universeGeneIds <- universeBuilder(p)

  ## We need the reverse mapping, the Entrez ids for all KEGG
  ## categories (in this subtree).
  keggcat.ent <- as.list(categoryToEntrezBuilder(p))
  keggcat.ent <- lapply(keggcat.ent, intersect, p@universeGeneIds)  

  ## Keep only genes that are in the universe
  p@geneIds <- lapply(p@geneIds, intersect, p@universeGeneIds)

  result <- lapply(p@geneIds, function(genes) {
    count <- sapply(keggcat.ent, function(x) sum(genes %in% x))
    my.keggcat.ent <- keggcat.ent[ count != 0 ]
    count <- count[ count != 0 ]
    size <- sapply(my.keggcat.ent, length)
    res <- .doHyperGTest(p, my.keggcat.ent, list(), genes)
    res <- data.frame(Pvalue=res$p, OddsRatio=res$odds,
                      ExpCount=res$expected, Count=count,
                      Size=size, row.names=names(res$p))
    if (p@drive) {
      drive <- lapply(my.keggcat.ent, intersect, genes)
      drive <- lapply(drive, paste, collapse=";")
      res$drive <- drive
    }
    
    res[ order(res$Pvalue), ]
  })

  new("KEGGListHyperGResult",
      reslist=result,
      annotation=p@annotation,
      geneIds=p@geneIds,
      testName=c("KEGG", "List"),
      testDirection=p@testDirection,
      pvalueCutoff=p@pvalueCutoff,
      drive=p@drive,
      universeGeneIds=p@universeGeneIds,
      catToGeneId=keggcat.ent)
}

#####################
## hyperGTest

setMethod("hyperGTest",
          signature(p="KEGGListHyperGParams"), isa.KEGGListHyperGTest)

setMethod("show", signature(object="KEGGListHyperGParams"),
          function(object) {
              cat("A", class(object), "instance\n")
              cat("  category:", object@categoryName, "\n")
              cat("annotation:", object@annotation, "\n")
          })

##################################################
## KEGGListHyperGResult
##################################################

setClass("KEGGListHyperGResult",
         contains="HyperGResultBase",
         representation=representation(
           reslist="list",
           drive="logical",
           universeGeneIds="character",
           catToGeneId="list"),
         prototype=prototype(
           testName="KEGG",
           reslist=list(),
           universeGeneIds=character(),
           catToGeneId=list()))

setMethod("show", signature(object="KEGGListHyperGResult"),
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

setMethod("summary", signature(object="KEGGListHyperGResult"),
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

setMethod("htmlReport", signature=(r="KEGGListHyperGResult"),
          function(r, file="", append=FALSE, label="", digits=3,
                   summary.args=NULL) {
            callNextMethod(r=r, file=file, append=append,
                           label=label, digits=digits,
                           summary.args=summary.args)            
          })

setMethod("pvalues", signature(r="KEGGListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$Pvalue, names=rownames(x))
          }))

setMethod("geneCounts", signature(r="KEGGListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$Count, names=rownames(x))
          }))

setMethod("oddsRatios", signature(r="KEGGListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$OddsRatio, names=rownames(x))
          }))

setMethod("expectedCounts", signature(r="KEGGListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$ExpCount, names=rownames(x))
          }))

setMethod("universeCounts", signature(r="KEGGListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$Size, names=rownames(x))
          }))

setMethod("universeMappedCount", signature(r="KEGGListHyperGResult"),
          function(r) length(r@universeGeneIds))

setMethod("geneMappedCount", signature(r="KEGGListHyperGResult"),
          function(r) sapply(r@geneIds, length))

setMethod("geneIdUniverse", signature(r="KEGGListHyperGResult"),
          function(r, cond=FALSE) r@catToGeneId)

setMethod("condGeneIdUniverse", signature(r="KEGGListHyperGResult"),
          function(r) geneIdUniverse(r, cond=TRUE))

## This function gives all the hits for the tested categories.
setMethod("geneIdsByCategory", signature(r="KEGGListHyperGResult"),
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

setMethod("sigCategories", signature(r="KEGGListHyperGResult"),
          function(r, p) {
            if (missing(p)) { p <- pvalueCutoff(r) }
            lapply(r@reslist, function(x) {
              rownames(x)[x$Pvalue < p]
            })
          })

ISA.KEGG <- function(modules,
                     org=getOrganism(modules),
                     shortorg=abbreviate(org, 2),
                     ann=annotation(modules),
                     features=featureNames(modules),
                     hgCutoff=0.001,
                     correction=TRUE, correction.method="holm") {

  isa2:::isa.status("Calculating KEGG enrichment", "in")
  
  require(paste(sep=".", "org", shortorg, "eg", "db"), character.only=TRUE)
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
