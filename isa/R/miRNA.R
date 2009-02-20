

##################################################
## miRNAListHyperGParams
##################################################

setClass("miRNAListHyperGParams",
         representation(conditional="logical"),
         contains="HyperGParams",
         prototype=prototype(categoryName=c("miRNA", "List"),
           conditional=FALSE))

##################
## makeValidParams

setMethod("makeValidParams", "miRNAListHyperGParams",
          function(object) {
            ## TODO: proper checks
            if (!is.list(object@geneIds)) {
              object@geneIds <- list(object@geneIds)
            }
            if (object@conditional) {
              stop("Conditional miRNA test is not implemented yet")
            }
            if (object@testDirection != "over") {
              stop("Only overrepresentation test is implemented yet")
            }
            object
          })

##################
## conditional

setMethod("conditional", "miRNAListHyperGParams", function(r) r@conditional)

##################
## conditional<-

setReplaceMethod("conditional", c("miRNAListHyperGParams", "logical"),
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
         signature(p="miRNAListHyperGParams"),
         function(p) {
           keep.all <- switch(testDirection(p),
                              over=FALSE,
                              under=TRUE,
                              stop("Bad testDirection slot"))

           genes <- unique(unlist(geneIds(p)))
           data(miRNA.mm, package="isa")
           cat.eg <- tapply(as.character(miRNA.mm[,1]),
                            as.character(miRNA.mm[,2]),
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
            data(miRNA.mm, package="isa")
            entrez <- unique(miRNA.mm[,1])
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
    res[ order(res$Pvalue), ]
  })

  new("miRNAListHyperGResult",
      reslist=result,
      annotation=p@annotation,
      geneIds=p@geneIds,
      testName=c("miRNA", "List"),
      testDirection=p@testDirection,
      pvalueCutoff=p@pvalueCutoff,
      conditional=p@conditional,
      universeGeneIds=p@universeGeneIds,
      catToGeneId=dbdcat.ent)
}

#####################
## hyperGTest

setMethod("hyperGTest",
          signature(p="miRNAListHyperGParams"), isa.miRNAListHyperGTest)

setMethod("show", signature(object="miRNAListHyperGParams"),
          function(object) {
              cat("A", class(object), "instance\n")
              cat("  category:", object@categoryName, "\n")
              cat("annotation:", object@annotation, "\n")
          })

##################################################
## miRNAListHyperGResult
##################################################

setClass("miRNAListHyperGResult",
         contains="HyperGResultBase",
         representation=representation(
           reslist="list",
           conditional="logical",
           universeGeneIds="character",
           catToGeneId="list"),
         prototype=prototype(
           testName="miRNA",
           reslist=list(),
           universeGeneIds=character(),
           catToGeneId=list()))

setMethod("show", signature(object="miRNAListHyperGResult"),
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

setMethod("summary", signature(object="miRNAListHyperGResult"),
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

setMethod("htmlReport", signature=(r="miRNAListHyperGResult"),
          function(r, file="", append=FALSE, label="", digits=3,
                   summary.args=NULL) {
            callNextMethod(r=r, file=file, append=append,
                           label=label, digits=digits,
                           summary.args=summary.args)            
          })

setMethod("pvalues", signature(r="miRNAListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$Pvalue, names=rownames(x))
          }))

setMethod("geneCounts", signature(r="miRNAListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$Count, names=rownames(x))
          }))

setMethod("oddsRatios", signature(r="miRNAListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$OddsRatio, names=rownames(x))
          }))

setMethod("expectedCounts", signature(r="miRNAListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$ExpCount, names=rownames(x))
          }))

setMethod("universeCounts", signature(r="miRNAListHyperGResult"),
          function(r) lapply(r@reslist, function(x) {
            structure(x$Size, names=rownames(x))
          }))

setMethod("universeMappedCount", signature(r="miRNAListHyperGResult"),
          function(r) length(r@universeGeneIds))

setMethod("geneMappedCount", signature(r="miRNAListHyperGResult"),
          function(r) sapply(r@geneIds, length))

setMethod("geneIdUniverse", signature(r="miRNAListHyperGResult"),
          function(r, cond=FALSE) r@catToGeneId)

setMethod("condGeneIdUniverse", signature(r="miRNAListHyperGResult"),
          function(r) geneIdUniverse(r, cond=TRUE))

## This function gives all the hits for the tested categories.
setMethod("geneIdsByCategory", signature(r="miRNAListHyperGResult"),
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

setMethod("sigCategories", signature(r="miRNAListHyperGResult"),
          function(r, p) {
            if (missing(p)) { p <- pvalueCutoff(r) }
            lapply(r@reslist, function(x) {
              rownames(x)[x$Pvalue < p]
            })
          })

isa.miRNA <- function(isaresult, organism=NULL, annotation=NULL, features=NULL,
                     hgCutoff=0.001, correction=TRUE) {

  if (is.null(organism)) organism <- isaresult$rundata$organism
  if (is.null(annotation)) annotation <- isaresult$rundata$annotation
  if (is.null(features)) features <- isaresult$rundata$features  

  if (organism != "Mus musculus") {
    stop("This method is only implemented for `Mus musculus'")
  }
  
  require(paste(sep="", annotation, ".db"), character.only=TRUE)
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
    try( new("miRNAListHyperGParams", geneIds = selectedEntrezIds,
             universeGeneIds = entrezUniverse, annotation = annotation,
             pvalueCutoff = hgCutoff, testDirection = "over") )

  cat(" -- Doing test\n")
  hgOver <- hyperGTest(params)

  hgOver
}

##########################################

convert.miRNA <- function(file) {
  
  tab <- read.delim(file, header=TRUE, comment.char="#")
  tab <- tab[ tab$Species.ID=="10090", ]
  tab <- tab[,2:1]
  tab <- unique(tab)
  tab <- as.matrix(tab)
  tab[] <- sub("^[ ]+", "", tab)
  tab[] <- sub("[ ]+$", "", tab)

  tab
}
    
