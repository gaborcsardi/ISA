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
