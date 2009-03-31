

##################################################
## CHRListHyperGParams
##################################################

setClass("CHRListHyperGParams",
         representation(),
         contains="HyperGParams",
         prototype=prototype(categoryName=c("CHR", "List")))

##################
## makeValidParams

setMethod("makeValidParams", "CHRListHyperGParams",
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
    res[ order(res$Pvalue), ]
  })

  new("CHRListHyperGResult",
      reslist=result,
      annotation=p@annotation,
      geneIds=p@geneIds,
      testName=c("CHR", "List"),
      testDirection=p@testDirection,
      pvalueCutoff=p@pvalueCutoff,
      universeGeneIds=p@universeGeneIds,
      catToGeneId=dbdcat.ent)
}

#####################
## hyperGTest

setMethod("hyperGTest",
          signature(p="CHRListHyperGParams"), isa.CHRListHyperGTest)

setMethod("show", signature(object="CHRListHyperGParams"),
          function(object) {
              cat("A", class(object), "instance\n")
              cat("  category:", object@categoryName, "\n")
              cat("annotation:", object@annotation, "\n")
          })

##################################################
## CHRListHyperGResult
##################################################

setClass("CHRListHyperGResult",
         contains="HyperGResultBase",
         representation=representation(
           reslist="list",
           universeGeneIds="character",
           catToGeneId="list"),
         prototype=prototype(
           testName="CHR",
           reslist=list(),
           universeGeneIds=character(),
           catToGeneId=list()))

setMethod("show", signature(object="CHRListHyperGResult"),
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

setMethod("summary", signature(object="CHRListHyperGResult"),
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

setMethod("htmlReport", signature=(r="CHRListHyperGResult"),
          function(r, file="", append=FALSE, label="", digits=3,
                   summary.args=NULL) {
            callNextMethod(r=r, file=file, append=append,
                           label=label, digits=digits,
                           summary.args=summary.args)            
          })

setMethod("pvalues", signature(r="CHRListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$Pvalue, names=rownames(x))
          }))

setMethod("geneCounts", signature(r="CHRListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$Count, names=rownames(x))
          }))

setMethod("oddsRatios", signature(r="CHRListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$OddsRatio, names=rownames(x))
          }))

setMethod("expectedCounts", signature(r="CHRListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$ExpCount, names=rownames(x))
          }))

setMethod("universeCounts", signature(r="CHRListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$Size, names=rownames(x))
          }))

setMethod("universeMappedCount", signature(r="CHRListHyperGResult"),
          function(r) length(r@universeGeneIds))

setMethod("geneMappedCount", signature(r="CHRListHyperGResult"),
          function(r) sapply(r@geneIds, length))

setMethod("geneIdUniverse", signature(r="CHRListHyperGResult"),
          function(r, cond=FALSE) r@catToGeneId)

setMethod("condGeneIdUniverse", signature(r="CHRListHyperGResult"),
          function(r) geneIdUniverse(r, cond=TRUE))

## This function gives all the hits for the tested categories.
setMethod("geneIdsByCategory", signature(r="CHRListHyperGResult"),
          function(r, catids=NULL) {
            lapply(r@geneIds, function(genes) {
              tmp <- lapply(r@catToGeneId, function(x) {
                genes [ genes %in% x ]
              })
              tmp <- tmp[ sapply(tmp, length) != 0 ]
              ord <- order(names(tmp))
              tmp <- tmp[ord]
            })
          })

setMethod("sigCategories", signature(r="CHRListHyperGResult"),
          function(r, p) {
            if (missing(p)) { p <- pvalueCutoff(r) }
            lapply(r@reslist, function(x) {
              rownames(x)[x$Pvalue < p]
            })
          })

isa.CHR <- function(isaresult, organism=NULL, annotation=NULL, features=NULL,
                     hgCutoff=0.001, correction=TRUE) {

  if (is.null(organism)) organism <- isaresult$rundata$organism
  if (is.null(annotation)) annotation <- isaresult$rundata$annotation
  if (is.null(features)) features <- isaresult$rundata$features  
  shortorganism <- abbreviate(organism, 2)

  require(paste(sep="", annotation, ".db"), character.only=TRUE)
  require(paste(sep=".", "org", shortorganism, "eg", "db"), character.only=TRUE)
  require(Category)

  ENTREZ <- get(paste(sep="", annotation, "ENTREZID"))

  cat(" -- Extracting Entrez genes\n")

  selectedEntrezIds <- lapply(seq_len(ncol(isaresult$genes)),
                              function(x) features[isaresult$genes[,x] != 0])
  selectedEntrezIds <- lapply(selectedEntrezIds,
                              function(x) unlist(mget(x, ENTREZ)))
  selectedEntrezIds <- lapply(selectedEntrezIds, unique)

  cat(" -- Extracting Entrez Universe\n")
  entrezUniverse <- unique(unlist(mget(features, ENTREZ)))

  params <-
    try( new("CHRListHyperGParams", geneIds = selectedEntrezIds,
             universeGeneIds = entrezUniverse, annotation = annotation,
             pvalueCutoff = hgCutoff, testDirection = "over") )

  cat(" -- Doing test\n")
  hgOver <- hyperGTest(params)

  hgOver
}

