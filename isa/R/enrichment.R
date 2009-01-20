
isa.GO <- function(isaresult, organism=NULL, annotation=NULL, features=NULL,
                   hgCutoff=0.001, correction=TRUE) {

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

  valid <- sapply(selectedEntrezIds, length) != 0

  cat(" -- Extracting Entrez Universe\n")
  entrezUniverse <- unique(unlist(mget(features, ENTREZ)))
  
  paramsBP <- paramsCC <- paramsMF <-
    new("GOHyperGParams", geneIds = character(),
        universeGeneIds = entrezUniverse, annotation = annotation,
        ontology = "BP", pvalueCutoff = hgCutoff, conditional = FALSE,
        testDirection = "over")

  paramsBP@geneIds <- selectedEntrezIds[valid]
  paramsCC@geneIds <- selectedEntrezIds[valid]
  paramsMF@geneIds <- selectedEntrezIds[valid]
  
  ontology(paramsCC) <- "CC"
  ontology(paramsMF) <- "MF"
  
  hgOverBP <- hgOverCC <- hgOverMF <-
    vector(mode="list", length=length(selectedEntrezIds))

  cat(" -- Doing BP test\n")
  hgOverBP[valid] <- QhyperGTest(paramsBP, correction=correction)
  cat(" -- Doing CC test\n")
  hgOverCC[valid] <- QhyperGTest(paramsCC, correction=correction)
  cat(" -- Doing MF test\n")
  hgOverMF[valid] <- QhyperGTest(paramsMF, correction=correction)
  
  hgOverBP[!valid] <- hgOverCC[!valid] <- hgOverMF[!valid] <- NA
  
  list(hgOverBP, hgOverCC, hgOverMF)
}
  
QhyperGTest <- function(params, correction) {

  ## These are not yet implemented
  if (params@testDirection == "under") {
    stop("Under-representation is not implemented yet")
  }
  if (params@conditional) {
    stop("Conditional tests are not implemented yet")
  }
  
  ## Check that we have the require Entrez <-> GO mappings,
  pkg <- sub(".db", "", params@datPkg@name, fixed=TRUE)
  organism <- get(paste(sep="", pkg, "ORGANISM"))
  organism <- abbreviate(organism, 2)

##   maps <- paste(sep=".", "org", organism,
##                 c("egGO", "egGO2ALLEGS", "egGOALLCC", "egGOALLBP", "egGOALLMF",
##                   "egGOHAVECC", "egGOHAVEBP", "egGOHAVEMF"))
##   if (any(! sapply(maps, exists))) {
##     stop("Some Entrez <-> GO mappings do not exist: ", sapply(maps,exists))
##   }

  ## Filter the gene universe to include genes that have at least one annotation
  ## in the current ontology
  havelist <- get(paste(sep="", "org.", organism, ".egGOHAVE", ontology(params)))
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
  all.egs <- get(paste(sep=".", "org", organism, "egGO2ALLEGS"))
  if (length(params@geneIds) > 3) {
    simplified <- TRUE
    egGO2ALLEGS.mine <- new.env()
    all <- as.list(all.egs)
    all <- lapply(all, function(x) intersect(x, params@universeGeneIds))
    for (n in seq(all)) {
      assign(names(all)[n], all[[n]], envir=egGO2ALLEGS.mine)
    }
  } else {
    simplified <- FALSE
    egGO2ALLEGS.mine <- all.egs
  }

  ## Ok, start testing
  all.cc <- get(paste(sep="", "org.", organism, ".egGOALL", ontology(params)))
      
  result <- lapply(params@geneIds, function(genes) {

    genes <- as.character(genes[!is.na(genes)])
    tmp <- mget(genes, all.cc, ifnotfound=NA)
    tmp <- tmp[ !is.na(tmp) ]
    
    tmp2 <- unique(as.character(unlist(unname(tmp))))
    tmp5 <- mget(tmp2, egGO2ALLEGS.mine)
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
  

isa.miRNA <- function(isaresult, organism=NULL, annotation=NULL, features=NULL,
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

  cat(" -- Extracting Entrez genes")
  
  selectedEntrezIds <- lapply(seq_len(ncol(isaresult$genes)),
                              function(x) features[isaresult$genes[,x] != 0])
  selectedEntrezIds <- lapply(selectedEntrezIds,
                              function(x) unlist(mget(x, ENTREZ)))
  selectedEntrezIds <- lapply(selectedEntrezIds, unique)

  cat(" -- Extracting Entrez Universe\n")
  entrezUniverse <- unique(unlist(mget(features, ENTREZ)))

  params <-
    try( new("miRNAHyperGParams", geneIds=character(),
             universeGeneIds = entrezUniverse, annotation=annotation,
             pvalueCutoff = hgCutoff, testDirection = "over") )

  valid <- sapply(selectedEntrezIds, length) != 0
  params@geneIds <- selectedEntrezIds[valid]

  hgOver <- vector(mode="list", length=length(selectedEntrezIds))

  cat(" -- Doing test\n")
  hgOver[valid] <- QhyperGTestmiRNA(params, correction=correction)
  
  hgOver
}
  
QhyperGTestmiRNA <- function(params, correction) {

  ## These are not yet implemented
  if (params@testDirection == "under") {
    stop("Under-representation is not implemented yet")
  }

  ## Check that we have the require Entrez <-> miRNA mappings,
  pkg <- sub(".db", "", params@datPkg@name, fixed=TRUE)
  organism <- get(paste(sep="", pkg, "ORGANISM"))
  organism <- abbreviate(organism, 2)
  
##   maps <- paste(sep=".", "org", organism,
##                 c("egmiRNA", "egmiRNA2EG", "egmiRNAHAVE"))
##   if (any(! sapply(maps, exists))) {
##     stop("Some Entrez <-> miRNA mappings do not exist: ", sapply(maps,exists))
##   }
  
  ## Filter the gene universe to include genes that have at least one annotation
  ## in the current ontology
  havelist <- get(paste(sep="", "org.", organism, ".egmiRNAHAVE"))
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
  all.egs <- get(paste(sep=".", "org", organism, "egmiRNA2EG"))
  if (length(params@geneIds) > 3) {
    simplified <- TRUE
    egmiRNA2EG.mine <- new.env()
    all <- as.list(all.egs)
    all <- lapply(all, function(x) intersect(x, params@universeGeneIds))
    for (n in seq(all)) {
      assign(names(all)[n], all[[n]], envir=egmiRNA2EG.mine)
    }
  } else {
    simplified <- FALSE
    egmiRNA2EG.mine <- all.egs
  }

  ## Ok, start testing
  all.cc <- get(paste(sep="", "org.", organism, ".egmiRNA"))
      
  result <- lapply(params@geneIds, function(genes) {

    genes <- as.character(genes[!is.na(genes)])
    tmp <- mget(genes, all.cc, ifnotfound=NA)
    tmp <- tmp[ !is.na(tmp) ]
    
    tmp2 <- unique(as.character(unlist(unname(tmp))))
    tmp5 <- mget(tmp2, egmiRNA2EG.mine)
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
  
