#####################################
## AllClasses.R

setClass("CHRHyperGParams",
         contains="HyperGParams",
         prototype=prototype(categoryName="CHR"))

setClass("CHRHyperGResult",
         contains="HyperGResult")

setMethod("categoryToEntrezBuilder",
          signature(p="CHRHyperGParams"),
          function(p) {
              getCHRToEntrezMap(p)
          })

#####################################
## categoryToEntrezBuilder-methods.R

getCHRToEntrezMap <- function(p) {
    keep.all <- switch(testDirection(p),
                       over=FALSE,
                       under=TRUE,
                       stop("Bad testDirection slot"))
    lib <- annotation(p)
    CHR2allprobes <- getDataEnv("CHR2PROBE", lib)
    probeAnnot <- getCHRToProbeMap(CHR2allprobes)
    probeToEntrezMapHelper(probeAnnot, geneIds(p), p@datPkg, universeGeneIds(p),
                           keep.all=keep.all)
}

getCHRToProbeMap <- function(CHR2allprobes, CHRIds) {
    probeAnnot = as.list(CHR2allprobes)
    if (!missing(CHRIds))
      probeAnnot = probeAnnot[CHRIds]
    removeLengthZeroAndMissing(probeAnnot)
}

####################################
## hyperGTest-methods.R

geneCHRHyperGeoTest <- function(entrezGeneIds, lib, universe=NULL)
{
    .Defunct("hyperGTest")
    if (missing(universe) || is.null(universe))
      universe <- character(0)
    params <- new("CHRHyperGParams",
                  geneIds=entrezGeneIds,
                  universeGeneIds=universe,
                  annotation=lib)
    hyperGTest(params)
}

#####################################
## summary-methods.R

setMethod("summary", signature(object="CHRHyperGResult"),
          function(object, pvalue=pvalueCutoff(object),
                   categorySize=NULL, htmlLinks=FALSE){
              CHR_URL <- "http://www.targetscan.org/cgi-bin/targetscan/vert_40/targetscan.cgi?species=%s&gid=&mir_c=%s&mir_sc=&mir_nc=&mirg="
              annOrg <- get(paste(annotation(object), "ORGANISM", sep=""))
              orgSpecifier <- switch(annOrg,
                                     "Homo sapiens"="Human",
                                     "Mus musculus"="Mouse",
                                     "Rattus norvegicus"="Rat",
                                     ## will need others in future
                                     "Human")
              df <- callNextMethod(object=object, pvalue=pvalue,
                                   categorySize=categorySize)
              if(nrow(df) == 0){
                  df$Term <- character(0)
                  return(df)
              }
##               CHRIds <- df[[1]]
##               keggEnv <- getAnnMap("PATHID2NAME", "KEGG", load=TRUE)
##               keggTerms <- unlist(mget(keggIds, keggEnv, ifnotfound=NA))
##               if(htmlLinks){
##                   keggIdUrls <- sapply(keggIds,
##                                        function(x)
##                                        sprintf(KEGG_URL, orgSpecifier, x))
##                   keggTerms <- paste('<a href="', keggIdUrls, '">', keggTerms,
##                                      '</a>', sep="")
##               }
##               df$Term <- keggTerms
              df
          })

setMethod("htmlReport", signature(r="CHRHyperGResult"),
          function(r, file="", append=FALSE, label="",
                   digits=3, summary.args=list(htmlLinks=TRUE)){
              callNextMethod(r=r, file=file, append=append,
                             label=label, digits=digits,
                             summary.args=summary.args)
          })

####################################
## universeBuilder-methods.R

setMethod("universeBuilder", signature(p="CHRHyperGParams"),
          function(p) {
                getUniverseViaCHR(p)
          })

getUniverseViaCHR <- function(p) {
    entrezIds <- universeGeneIds(p)
    probe2CHR <- as.list(getDataEnv("CHR", annotation(p)))
    notNA <- sapply(probe2CHR, function(x) !(length(x) == 1 && is.na(x)))
    probe2CHR <- probe2CHR[notNA]
    probes <- names(probe2CHR)
    getUniverseHelper(probes, p@datPkg, universeGeneIds(p))
}

isa.CHR <- function(isaresult, organism=NULL, annotation=NULL, features=NULL,
                    hgCutoff=0.001, correction=TRUE) {

  if (is.null(organism)) organism <- isaresult$rundata$organism
  if (is.null(annotation)) annotation <- isaresult$rundata$annotation
  if (is.null(features)) features <- isaresult$rundata$features  
  shortorganism <- abbreviate(organism, 2)
  require(paste(sep=".", "org", shortorganism, "eg", "db"), character.only=TRUE)
  
  require(paste(sep="", annotation, ".db"), character.only=TRUE)
  require(annotate)
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
    try( new("CHRHyperGParams", geneIds=character(),
             universeGeneIds = entrezUniverse, annotation=annotation,
             pvalueCutoff = hgCutoff, testDirection = "over") )

  valid <- sapply(selectedEntrezIds, length) != 0
  params@geneIds <- selectedEntrezIds[valid]

  hgOver <- vector(mode="list", length=length(selectedEntrezIds))

  cat(" -- Doing test\n")
  hgOver[valid] <- QhyperGTestCHR(params, correction=correction)
  
  hgOver
}

QhyperGTestCHR <- function(params, correction) {

  ## These are not yet implemented
  if (params@testDirection == "under") {
    stop("Under-representation is not implemented yet")
  }

  ## Check that we have the require Entrez <-> CHR mappings,
  pkg <- sub(".db", "", params@datPkg@name, fixed=TRUE)
  organism <- get(paste(sep="", pkg, "ORGANISM"))
  organism <- abbreviate(organism, 2)
  
##   maps <- paste(sep=".", "org", organism,
##                 c("egCHR", "egCHR2EG", "egCHRHAVE"))
##   if (any(! sapply(maps, exists))) {
##     stop("Some Entrez <-> CHR mappings do not exist: ", sapply(maps,exists))
##   }
  
  ## Filter the gene universe to include genes that have at least one annotation
  ## in the current ontology
  havelist <- get(paste(sep="", "org.", organism, ".egCHRHAVE"))
  if (length(params@universeGeneIds)==0) {
    params@univerGeneIds <- havelist
  } else { 
    params@universeGeneIds <- intersect(havelist, params@universeGeneIds)
  }
  
  ## Pre-generate the filtered list of probes, only include those that are
  ## in the universe
  if (!is.list(params@geneIds)) {
    params@geneIds <-list(params@geneIds)
  }
  all.egs <- get(paste(sep=".", "org", organism, "egCHR2EG"))
  if (length(params@geneIds) > 3) {
    simplified <- TRUE
    egCHR2EG.mine <- new.env()
    all <- as.list(all.egs)
    all <- lapply(all, function(x) intersect(x, params@universeGeneIds))
    for (n in seq(all)) {
      assign(names(all)[n], all[[n]], envir=egCHR2EG.mine)
    }
  } else {
    simplified <- FALSE
    egCHR2EG.mine <- all.egs
  }

  ## Ok, start testing
  all.cc <- get(paste(sep="", "org.", organism, ".egCHR"))
      
  result <- lapply(params@geneIds, function(genes) {

    genes <- as.character(genes[!is.na(genes)])
    tmp <- mget(genes, all.cc, ifnotfound=NA)
    tmp <- tmp[ !is.na(tmp) ]
    
    tmp2 <- unique(as.character(unlist(unname(tmp))))
    tmp5 <- mget(tmp2, egCHR2EG.mine)
    if (!simplified) {
      tmp5 <- lapply(tmp5, function(x) intersect(x, params@universeGeneIds))
    }

    selected <- genes[ genes %in% params@universeGeneIds ]
    res <- .doHyperGTest(params, tmp5, list(), selected)
    res$size <- sapply(tmp5, length)
    res$count <- sapply(tmp5, function(x) sum(selected %in% x))
    drive <- lapply(tmp5, function(x) genes[which(genes %in% x)])
    res$drive <- sapply(drive, paste, collapse=";")

    if (correction && length(res$p) >0) {
      res$p <- res$p * length(res$size)
      res$p <- pmin(res$p, 1)
    }
    
    res <- data.frame(Pvalue=res$p, OddsRatio=res$odds, ExpCount=res$expected,
                      Count=res$count, Size=res$size, Drive=res$drive,
                      row.names=names(res$p))
    res[ order(res$Pvalue), ]
  })

  result
}
  

