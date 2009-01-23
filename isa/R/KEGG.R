  
isa.KEGG <- function(isaresult, organism=NULL, annotation=NULL, features=NULL,
                     hgCutoff=0.001, correction=TRUE) {

  if (is.null(organism)) organism <- isaresult$rundata$organism
  if (is.null(annotation)) annotation <- isaresult$rundata$annotation
  if (is.null(features)) features <- isaresult$rundata$features  
  shortorganism <- abbreviate(organism, 2)
  require(paste(sep=".", "org", shortorganism, "eg", "db"), character.only=TRUE)
  
  require(paste(sep="", annotation, ".db"), character.only=TRUE)
  require(annotate)
  require(Category)
  require(KEGG.db)

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
    try( new("KEGGHyperGParams", geneIds = character(),
             universeGeneIds = entrezUniverse, annotation = annotation,
             pvalueCutoff = hgCutoff, testDirection = "over") )

  valid <- sapply(selectedEntrezIds, length) != 0
  params@geneIds <- selectedEntrezIds[valid]

  hgOver <- vector(mode="list", length=length(selectedEntrezIds))

  cat(" -- Doing test\n")
  hgOver[valid] <- QhyperGTestKEGG(params, correction=correction)

  hgOver
}

QhyperGTestKEGG <- function(params, correction) {

  ## These are not yet implemented
  if (params@testDirection == "under") {
    stop("Under-representation is not implemented yet")
  }
  
  ## Check that we have the require Entrez <-> GO mappings,
  pkg <- sub(".db", "", params@datPkg@name, fixed=TRUE)
  organism <- get(paste(sep="", pkg, "ORGANISM"))
  organism <- abbreviate(organism, 2)

##   maps <- paste(sep=".", "org", organism,
##                 c("egPATH", "egPATH2EG", "egPATHHAVE"))
##   if (any(! sapply(maps, exists))) {
##     stop("Some Entrez <-> KEGG mappings do not exist: ", sapply(maps,exists))
##  }

  ## Filter the gene universe to include genes that have at least one annotation
  ## in the current ontology
  havelist <- get(paste(sep="", "org.", organism, ".egPATHHAVE"))
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
  all.egs <- get(paste(sep=".", "org", organism, "egPATH2EG"))
  if (length(params@geneIds) > 3) {
    simplified <- TRUE
    egPATH2EG.mine <- new.env()
    all <- as.list(all.egs)
    all <- lapply(all, function(x) intersect(x, params@universeGeneIds))
    for (n in seq(all)) {
      assign(names(all)[n], all[[n]], envir=egPATH2EG.mine)
    }
  } else {
    simplified <- FALSE
    egPATH2EG.mine <- all.egs
  }

  ## Ok, start testing
  all.cc <- get(paste(sep="", "org.", organism, ".egPATH"))
      
  result <- lapply(params@geneIds, function(genes) {

    genes <- as.character(genes[!is.na(genes)])
    tmp <- mget(genes, all.cc, ifnotfound=NA)
    tmp <- tmp[ !is.na(tmp) ]
    
    tmp2 <- unique(as.character(unlist(unname(tmp))))
    tmp5 <- mget(tmp2, egPATH2EG.mine)
    if (!simplified) {
      tmp5 <- lapply(tmp5, function(x) intersect(x, params@universeGeneIds))
    }

    selected <- genes[ genes %in% params@universeGeneIds ]
    res <- .doHyperGTest(params, tmp5, list(), selected)
    res$size <- sapply(tmp5, length)
    res$count <- sapply(tmp5, function(x) sum(selected %in% x))
    drive <- lapply(tmp5, function(x) genes[which(genes %in% x)])
    res$drive <- sapply(drive, paste, collapse=";")

    if (correction && length(res$p)>0) {
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
