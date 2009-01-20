#####################################
## AllClasses.R

setClass("miRNAHyperGParams",
         contains="HyperGParams",
         prototype=prototype(categoryName="miRNA"))

setClass("miRNAHyperGResult",
         contains="HyperGResult")

setMethod("categoryToEntrezBuilder",
          signature(p="miRNAHyperGParams"),
          function(p) {
              getmiRNAToEntrezMap(p)
          })

#####################################
## categoryToEntrezBuilder-methods.R

getmiRNAToEntrezMap <- function(p) {
    keep.all <- switch(testDirection(p),
                       over=FALSE,
                       under=TRUE,
                       stop("Bad testDirection slot"))
    lib <- annotation(p)
    miRNA2allprobes <- getDataEnv("miRNA2PROBE", lib)
    probeAnnot <- getmiRNAToProbeMap(miRNA2allprobes)
    probeToEntrezMapHelper(probeAnnot, geneIds(p), p@datPkg, universeGeneIds(p),
                           keep.all=keep.all)
}

getmiRNAToProbeMap <- function(miRNA2allprobes, miRNAIds) {
    probeAnnot = as.list(miRNA2allprobes)
    if (!missing(miRNAIds))
      probeAnnot = probeAnnot[miRNAIds]
    removeLengthZeroAndMissing(probeAnnot)
}

####################################
## hyperGTest-methods.R

genemiRNAHyperGeoTest <- function(entrezGeneIds, lib, universe=NULL)
{
    .Defunct("hyperGTest")
    if (missing(universe) || is.null(universe))
      universe <- character(0)
    params <- new("miRNAHyperGParams",
                  geneIds=entrezGeneIds,
                  universeGeneIds=universe,
                  annotation=lib)
    hyperGTest(params)
}

#####################################
## summary-methods.R

setMethod("summary", signature(object="miRNAHyperGResult"),
          function(object, pvalue=pvalueCutoff(object),
                   categorySize=NULL, htmlLinks=FALSE){
              miRNA_URL <- "http://www.targetscan.org/cgi-bin/targetscan/vert_40/targetscan.cgi?species=%s&gid=&mir_c=%s&mir_sc=&mir_nc=&mirg="
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
##               miRNAIds <- df[[1]]
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

setMethod("htmlReport", signature(r="miRNAHyperGResult"),
          function(r, file="", append=FALSE, label="",
                   digits=3, summary.args=list(htmlLinks=TRUE)){
              callNextMethod(r=r, file=file, append=append,
                             label=label, digits=digits,
                             summary.args=summary.args)
          })

####################################
## universeBuilder-methods.R

setMethod("universeBuilder", signature(p="miRNAHyperGParams"),
          function(p) {
                getUniverseViamiRNA(p)
          })

getUniverseViamiRNA <- function(p) {
    entrezIds <- universeGeneIds(p)
    probe2miRNA <- as.list(getDataEnv("miRNA", annotation(p)))
    notNA <- sapply(probe2miRNA, function(x) !(length(x) == 1 && is.na(x)))
    probe2miRNA <- probe2miRNA[notNA]
    probes <- names(probe2miRNA)
    getUniverseHelper(probes, p@datPkg, universeGeneIds(p))
}
