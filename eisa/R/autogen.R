
ISA.html <- function(...) {
  ## TODO: call ISA.html.table and ISA.html.modules
}

isa.autogen.create.dirs <- function(template, target.dir) {
  
  if (file.exists(target.dir)) {
    if (!file.info(target.dir)$isdir) {
      stop("Target exists and it is not a directory")
    }
  } else {
    dir.create(target.dir)
  }

  imagedir <- paste(target.dir, sep="/", "images")
  if (file.exists(imagedir)) {
    if (!file.info(imagedir)$isdir) {
      stop("Target (images) exists and it is not a directory")
    }
  } else {
    dir.create(imagedir)    
  }

  ff <- list.files(template)
  
  for (f in ff) {
    file.copy(paste(sep="", template, "/", f),
              paste(sep="", target.dir, "/", f) )
  }
  ff <- list.files(paste(sep="/", template, "images"))

  for (f in ff) {
    file.copy(paste(sep="", template, "/images/", f),
              paste(sep="", target.dir, "/images/", f) )
  }
}

ISA.html.table <- function(modules, target.dir,
                           which=seq_len(length(modules)),
                           template=system.file("autogen", package="eisa"),
                           GO=NULL, KEGG=NULL, miRNA=NULL, CHR=NULL, DBD=NULL,
                           htmltitle=NULL, notes=NULL, seed=NULL) {

  isa2:::isa.status("Creating HTML module table", "in")

  require(GO.db)
  require(KEGG.db)

  isa.autogen.create.dirs(template, target.dir)
  
  db <- GO_dbconn()
  query <- "SELECT go_id, term, definition FROM go_term"
  go.terms <- dbGetQuery(db, query)
  rownames(go.terms) <- go.terms[,1]
  go.terms <- go.terms[,-1]
  
  options(digits=2)
  
  f <- function(obj, pvalue=0.05, type) {

    pv <- sapply(pvalues(obj), function(x) if (length(x) >= 1) x[1] else 1.0)
    uc <- ifelse(pv <= pvalue, format(sapply(universeCounts(obj), "[", 1)), "")
    ec <- ifelse(pv <= pvalue, format(sapply(expectedCounts(obj), "[", 1)), "")
    gc <- ifelse(pv <= pvalue, format(sapply(geneCounts(obj), "[", 1)), "")
    ca <- names(pv)
    ca <- switch(type,
         "GO"=go.terms$term[ match(ca,rownames(go.terms)) ],
         "KEGG"=as.character(mget(ca, KEGGPATHID2NAME)),
                 ca)

    res <- paste(sep="", ca, " <span class=\"pvalue\"><br/>(", format(pv), "/", ec,
                 "/", gc, "/", uc, ")</span>")
    ifelse(pv <= pvalue, res, "")
  }
  
  tables.BP <- f(GO$BP, type="GO")[which]
  tables.CC <- f(GO$CC, type="GO")[which]
  tables.MF <- f(GO$MF, type="GO")[which]
  tables.KEGG <- f(KEGG, type="KEGG")[which]
  tables.miRNA <- if (!is.null(miRNA)) f(miRNA, type="miRNA")[which] else ""
  tables.DBD <- if (!is.null(DBD)) f(DBD, type="DBD")[which] else ""
  tables.CHR <- if (!is.null(CHR)) f(CHR, type="CHR")[which] else ""

  thr <- paste(sep="/", featureThreshold(modules)[which],
               sampleThreshold(modules)[which])
  no.genes <- getNoFeatures(modules)
  no.conds <- getNoSamples(modules)

  #############

  head <- c("#", "Thr.", "#G", "#C", "GO BP", "GO CC", "GO MF", "KEGG")

  if (!is.null(seed)) {
    head <- c(head[1], "Seed", head[2:length(head)])
    seed <- paste(sep="", "<td>", seed, "</td>")
  } else {
    seed <- ""
  }
  
  if (length(tables.miRNA) != 1 || tables.miRNA != "") {
    tables.miRNA <- paste(sep="", "<td>", tables.miRNA, "</td>")
    head <- c(head, "miRNA")
  }
  if (length(tables.DBD) != 1 || tables.DBD != "") {
    tables.DBD <- paste(sep="", "<td>", tables.DBD, "</td>")
    head <- c(head, "DBD")
  }
  if (length(tables.CHR) != 1 || tables.CHR != "") {
    tables.CHR <- paste(sep="", "<td>", tables.CHR, "</td>")
    head <- c(head, "CHR")
  }
  
  head <- paste(sep="", collapse="", "<td>", head, "</td>")
  head <- paste(collapse="", "<tr>", head, "</tr>")
  table <- paste(sep="",
                 '<tr onclick="location.href=\'module-', which, '.html\'">',
                 '<td><a href="module-', which, '.html">', which, "</a></td>",
                 seed,
                 "<td>", thr, "</td>",
                 "<td>", no.genes, "</td>",
                 "<td>", no.conds, "</td>",
                 "<td>", tables.BP, "</td>",
                 "<td>", tables.CC, "</td>",
                 "<td>", tables.MF, "</td>",
                 "<td>", tables.KEGG, "</td>",
                 tables.miRNA,
                 tables.DBD,
                 tables.CHR,
                 "</tr>")

  color.table <- function(t) {
    ## colorify every second row, plus the first one
    t[1] <- sub('<tr', '<tr class="head"', t[1])
    if (length(t) > 2) {
      idx <- seq(3, length(t), by=2)
      t[ idx ] <- sub('<tr', '<tr class="even"', t[ idx ])
    }
    t
  }
  
  table <- c(head, table)
  table <- color.table(table)
  table <- paste(collapse="\n", table)
  
  table <- paste(collapse="",
                 "<table>",
                 table,
                 "</table>")
  
  lines <- readLines(paste(sep="", template, "/maintable.html.in"))
  
  ## title
  if (is.null(htmltitle)) {
    htmltitle <- "Transcription modules"
  }
  lines[ grep("<!-- title -->", lines) ] <- htmltitle

  if (is.character(notes)) {
    lines[ grep("<!-- notes -->", lines) ] <- notes
  }

  ## The table
  lines[ grep("<!-- table -->", lines) ] <- table
  
  fname <- paste(sep="", target.dir, "/maintable.html")
  cat(lines, file=fname, sep="\n")

  if (!file.exists(paste(sep="", target.dir, "/maintree.html"))) {
    file.symlink("maintable.html", paste(sep="", target.dir, "/maintree.html"))
  }
  if (!file.exists(paste(sep="", target.dir, "/index.html"))) {
    file.symlink("maintable.html", paste(sep="", target.dir, "/index.html"))
  }

  isa2:::isa.status("DONE", "out")
  
  invisible(NULL)
}  
                          

ISA.html.modules <- function(eset, modules, which=seq_len(length(modules)),
                             target.dir,
                             template=system.file("autogen", package="eisa"),
                             GO=NULL, KEGG=NULL, miRNA=NULL, CHR=NULL, DBD=NULL,
                             cond.to.include=NULL,
                             cond.col="white",
                             sep=NULL, seed=NULL) {

  isa2:::isa.status("Generating module pages", "in")
  
  isa.autogen.create.dirs(template, target.dir)

  drive.BP <- drive.CC <- drive.MF <- drive.KEGG <- drive.miRNA <-
    drive.DBD <- drive.CHR <- NULL
  if (!is.null(GO)) drive.BP <- geneIdsByCategory(GO$BP)
  if (!is.null(GO)) drive.CC <- geneIdsByCategory(GO$CC)
  if (!is.null(GO)) drive.MF <- geneIdsByCategory(GO$MF)
  if (!is.null(KEGG)) drive.KEGG <- geneIdsByCategory(KEGG)
  if (!is.null(miRNA)) drive.miRNA <- geneIdsByCategory(miRNA) 
  if (!is.null(DBD)) drive.DBD <- geneIdsByCategory(DBD) 
  if (!is.null(CHR)) drive.CHR <- geneIdsByCategory(CHR) 
  
  ## Then generate modules
  for (i in seq_along(which)) {
    x <- which[i]
    nx <- if (i!=length(which)) which[i+1] else which[1]
    px <- if (i!=1) which[i-1] else which[length(which)]
    isa.autogen.module(eisa.get.nm(eset, modules), modules, x,
                       target.dir=target.dir, template=template,
                       GO=GO, KEGG=KEGG, miRNA=miRNA, DBD=DBD, CHR=CHR,
                       cond.to.include=cond.to.include,
                       cond.col=cond.col, sep=sep,
                       seed=seed, drive.BP=drive.BP, drive.CC=drive.CC,
                       drive.MF=drive.MF, drive.KEGG=drive.KEGG,
                       drive.miRNA=drive.miRNA, drive.DBD=drive.DBD,
                       drive.CHR=drive.CHR,
                       next.module=nx, prev.module=px)
  }

  isa2:::isa.status("DONE", "out")
  
  invisible(NULL)
}

isa.autogen.module <- function(eset, modules, which, target.dir, template,
                               GO, KEGG, miRNA, CHR, DBD, cond.to.include,
                               cond.col="white", sep=NULL,
                               seed=NULL, drive.BP=NULL, drive.CC=NULL,
                               drive.MF=NULL, drive.KEGG=NULL,
                               drive.miRNA=NULL, drive.DBD=NULL,
                               drive.CHR=NULL,
                               next.module=NULL, prev.module=NULL) {

  isa2:::isa.status(paste("Generating HTML page for module", which), "in")

  if (require(Cairo)) { png <- CairoPNG }
  require(Biobase)
  require(igraph)
  require(xtable)

  nm <- list(Er=t(feat.exprs(eset)),
             Ec=samp.exprs(eset))
  
  nexp <- nm$Ec

  if (is.null(drive.BP) && !is.null(GO)) drive.BP <- geneIdsByCategory(GO$BP)
  if (is.null(drive.CC) && !is.null(GO)) drive.CC <- geneIdsByCategory(GO$CC)
  if (is.null(drive.MF) && !is.null(GO)) drive.MF <- geneIdsByCategory(GO$MF)
  if (is.null(drive.KEGG) && !is.null(KEGG)) drive.KEGG <- geneIdsByCategory(KEGG)
  if (is.null(drive.miRNA) && !is.null(miRNA)) drive.miRNA <-
    geneIdsByCategory(miRNA) 
  if (is.null(drive.DBD) && !is.null(DBD)) drive.DBD <- geneIdsByCategory(DBD) 
  if (is.null(drive.CHR) && !is.null(CHR)) drive.CHR <- geneIdsByCategory(CHR) 
  
  chip <- annotation(modules)
  library(paste(sep="", chip, ".db"),  character.only=TRUE)
  organism <- getOrganism(modules)
  short.organism <- organism
  require(paste(sep="", "org.", abbreviate(organism, 2), ".eg.db"),
          character.only=TRUE)
  SYMBOL <- get( paste(sep="", "org.", abbreviate(organism, 2), ".egSYMBOL") )
  ENTREZ <- get( paste(sep="", chip, "ENTREZID") )

  library(paste(sep="", "org.", abbreviate(organism,2), ".eg.db"),
          character.only=TRUE)

  ####################
  # create go.terms object
  
  db <- GO_dbconn()
  query <- "SELECT go_id, term, definition FROM go_term"
  go.terms <- dbGetQuery(db, query)
  rownames(go.terms) <- go.terms[,1]
  go.terms <- go.terms[,-1]

  ####################
  # Create GOGRAPHS object

  go.graph <- function(cats) {
    query <- paste("SELECT id1.go_id, id2.go_id, par.relationship_type FROM",
                   " go_", tolower(cats), "_parents AS par,",
                   " go_term AS id1, go_term AS id2",
                   " WHERE par._id==id1._id AND par._parent_id==id2._id",
                   sep="")
    parents <- dbGetQuery(db, query)
    colnames(parents) <- c("from", "to", "relationship")
    go.graph <- graph.data.frame(parents, directed=TRUE)
    go.graph
  }
  GOGRAPHS <- list(BP=go.graph("BP"),
                   CC=go.graph("CC"),
                   MF=go.graph("MF"))
  
  color.table <- function(t) {
    ## colorify every second row, plus the first one
    t[1] <- sub('<tr>', '<tr class="head">', t[1])
    if (length(t) > 2) {
      idx <- seq(3, length(t), by=2)
      t[ idx ] <- sub('<tr>', '<tr class="even">', t[ idx ])
    }
    t
  }

  f <- function(obj, pvalue=0.05, maxlines=NA, cat=NULL, drive=NULL) {
    if (class(obj)=="NULL") {
      return ("No enriched GO categories")
    } else {
      pval <- obj$Pvalue
      names(pval) <- rownames(obj)
      v <- pval <= pvalue
      if (sum(v)==0) { return("No enriched GO categories") }
      if (is.na(maxlines) || maxlines>sum(v)) { maxlines <- sum(v) }
      pval <- pval[v][1:maxlines]
      uc <- unname(obj$Size)[v][1:maxlines]
      ec <- unname(obj$ExpCount)[v][1:maxlines]
      gc <- unname(obj$Count)[v][1:maxlines]
      ca <- rownames(obj)[v][1:maxlines]
      nn <- go.terms[ca,]$term
    }
    
    df <- data.frame(Pvalue=pval, ECount=round(ec, 2), Count=gc, Size=uc,
                     Term=nn)

    drive <- drive[v]
    drive <- lapply(drive, function(x) unname(unlist(mget(x, SYMBOL, ifnotfound=NA))))
    if (any(sapply(drive, function(x) any(is.na(x))))) {
      warning("Some genes not found, inconsistent annotation packages")
      drive <- lapply(drive, function(x) x[!is.na(x)])
    }
    drive <- lapply(drive, sort)
    drive <- lapply(drive, paste, collapse=", ")
    df$Count <- paste(sep="", '<a href="#" onclick="togglestuff2(\'d.', cat, '.', seq(along=df[,1]),
                      '\'); return false;">', df$Count, '</a><br/><span id="d.', cat, '.', seq(along=df[,1]),
                      '" class="d.', cat, '" style="font-size:0.8em;display:none;visibility:hidden;">', drive,
                      '</span>')
  
    xdf <- xtable(df, display=c("s", "e", "g", "g", "d", "s"),
                  digits=c(NA, 3, 4, 4, 4, NA))
    tfname <- tempfile()
    zz <- file(tfname, "w")
    print(xdf, type="html", file=zz)
    close(zz)
    foo <- readLines(tfname)
    unlink(tfname)
    foo <- foo[4:(length(foo)-1)]

    foo <- fix.xtable(foo)
    
    link <- "http://www.godatabase.org/cgi-bin/amigo/go.cgi?view=details&amp;search_constraint=terms&amp;depth=0&amp;query="
    foo <- sub("(<td[^>]*>[ ]*)(GO:[0-9]+)",
               paste(sep="", '\\1<a href="', link, '\\2"> \\2 </a>'), foo)
    
    foo <- sub("<th> Count </th>",
               paste(sep="",
                     '<th> <a href="#" onclick="togglestuff3(\'d.', cat,
                     '\');return false;"> Count </a> </th>'),
               foo, fixed=TRUE)
    
    foo <- color.table(foo)
    paste(foo, collapse="\n")
  }

  tables.BP <- f(GO[[1]]@reslist[[which]], pvalue=0.05, maxlines=NA,
                 drive=drive.BP[[which]], cat="BP")
  tables.CC <- f(GO[[2]]@reslist[[which]], pvalue=0.05, maxlines=NA,
                 drive=drive.CC[[which]], cat="CC")
  tables.MF <- f(GO[[3]]@reslist[[which]], pvalue=0.05, maxlines=NA,
                 drive=drive.MF[[which]], cat="MF")

  g <- function(obj, pvalue=0.05, maxlines=NA, drive=NULL) {
    if (class(obj)=="NULL") {
      return ("No enriched GO categories")
    } else {
      pval <- obj$Pvalue
      names(pval) <- rownames(obj)
      v <- pval <= pvalue
      if (sum(v)==0) { return("No enriched KEGG pathways") }
      if (is.na(maxlines) || maxlines>sum(v)) { maxlines <- sum(v) }
      pval <- pval[v][1:maxlines]
      uc <- unname(obj$Size)[v][1:maxlines]
      ec <- unname(obj$ExpCount)[v][1:maxlines]
      gc <- unname(obj$Count)[v][1:maxlines]
      ca <- rownames(obj)[v][1:maxlines]
      nn <- unname(unlist(mget(ca, KEGGPATHID2NAME)))
    }

    df <- data.frame(Pvalue=pval, ECount=round(ec, 2), Count=gc, Size=uc,
                     Term=nn)
    cat <- "ke"

    drive0 <- drive[v]
    drive <- lapply(drive0, function(x) unname(unlist(mget(x, SYMBOL))))
    drive <- lapply(drive, sort)
    drive2 <- lapply(drive, paste, collapse=", ")
    df$Count <- paste(sep="", '<a href="#" onclick="togglestuff2(\'d.', cat, '.', seq(along=df[,1]),
                      '\'); return false;">', df$Count, '</a><br/><span id="d.', cat, '.', seq(along=df[,1]),
                      '" class="d.', cat, '" style="font-size:0.8em;display:none;visibility:hidden;">', drive2,
                      '</span>')
    
    xdf <- xtable(df, display=c("s", "e", "g", "g", "d", "s"),
                  digits=c(NA, 3, 4, 4, 4, NA))
    tfname <- tempfile()
    zz <- file(tfname, "w")
    print(xdf, type="html", file=zz)
    close(zz)
    foo <- readLines(tfname)
    unlink(tfname)
    foo <- foo[4:(length(foo)-1)]

    keggorg <- switch(organism, "Homo sapiens"="hsa", "Mus musculus"="mmu")
    
    foo <- fix.xtable(foo)

    link <- c("http://www.genome.jp/kegg-bin/mark_pathway_www?@", "/default%3dyellow/")
    foo <- sub("(<td[^>]*>[ ]*)([0-9]+)",
               paste(sep="", '\\1<a href="', link[1], keggorg,
                     '\\2', link[2], '"> \\2 </a>'), foo)

    ## mark genes on pathway, we still have 'drive0'
    drive2 <- sapply(drive0, paste, collapse="/")
    for (i in seq_along(foo)[-1]) {
      foo[[i]] <- sub(link[2], paste(sep="", link[2], drive2[[i-1]]),
                      foo[[i]], fixed=TRUE)
    }
    
    foo <- sub("<th> Count </th>",
               paste(sep="",
                     '<th> <a href="#" onclick="togglestuff3(\'d.', cat,
                     '\');return false;"> Count </a> </th>'),
               foo, fixed=TRUE)
    
    foo <- color.table(foo)
    paste(foo, collapse="\n")
  }

  require(KEGG.db)
  tables.KEGG <- g(KEGG@reslist[[which]], pvalue=0.05, maxlines=NA,
                   drive=drive.KEGG[[which]])
  
  if (!is.null(miRNA)) {

    h <- function(obj, pvalue=0.05, maxlines=NA, drive=NULL) {
      if (nrow(obj)==0) { return("<tr><td>No enriched miRNA families</td></tr>") } 
      pval <- obj$Pvalue
      v <- pval <= pvalue
      if (sum(v)==0) { return("<tr><td>No enriched miRNA families</td></tr>") }
      if (is.na(maxlines) || maxlines>sum(v)) { maxlines <- sum(v) }
      pval <- pval[v][1:maxlines]
      uc <- unname(obj$Size)[v][1:maxlines]
      ec <- unname(obj$ExpCount)[v][1:maxlines]
      gc <- unname(obj$Count)[v][1:maxlines]
      ca <- rownames(obj)[v][1:maxlines]
      df <- data.frame(Pvalue=pval, ECount=round(ec, 2), Count=gc, Size=uc)
      rownames(df) <- ca
      
      cat <- "mr"
      drive <- drive[v]      
      drive <- lapply(drive, function(x) unname(unlist(mget(x, SYMBOL))))
      drive <- lapply(drive, sort)
      drive <- lapply(drive, paste, collapse=", ")
      df$Count <- paste(sep="", '<a href="#" onclick="togglestuff2(\'d.', cat, '.', seq(along=df[,1]),
                        '\'); return false;">', df$Count, '</a><br/><span id="d.', cat, '.', seq(along=df[,1]),
                        '" class="d.', cat, '" style="font-size:0.8em;display:none;visibility:hidden;">', drive,
                          '</span>')
      
      xdf <- xtable(df, display=c("s", "e", "g", "d", "d"),
                    digits=c(NA, 3, 4, 4, 4))
      tfname <- tempfile()
      zz <- file(tfname, "w")
      sink(zz)
      print(xdf, "html")
      sink() 
      close(zz)
      foo <- readLines(tfname)
      unlink(tfname)
      foo <- foo[4:(length(foo)-1)]

      foo <- fix.xtable(foo)

      link <- c("http://www.targetscan.org/cgi-bin/targetscan/vert_40/targetscan.cgi?species=", "&amp;gid=&amp;mir_c=", "&amp;mir_sc=&amp;mir_nc=&amp;mirg=")
      foo <- sub("(<td[^>]*>[ ]*)([^< ]+)",
                 paste(sep="", '\\1<a href="', link[1], short.organism, link[2],
                       '\\2', link[3], '"> ', '\\2 </a>'), foo)
      
      foo <- sub("<th> Count </th>",
                 paste(sep="",
                       '<th> <a href="#" onclick="togglestuff3(\'d.', cat,
                       '\');return false;"> Count </a> </th>'),
                 foo, fixed=TRUE)
      
      foo <- color.table(foo)
      paste(foo, collapse="\n")
    }
    
    tables.miRNA <- h(miRNA@reslist[[which]], pvalue=0.05, maxlines=NA,
                      drive=drive.miRNA[[which]])
  } else {
    tables.miRNA <- "<p>Not tested</p>"
  }

  if (!is.null(DBD)) {

    h <- function(obj, pvalue=0.05, maxlines=NA, drive=NULL) {
      if (nrow(obj)==0) { return("<tr><td>No enriched DBD TFs</td></tr>") } 
      pval <- obj$Pvalue
      v <- pval <= pvalue
      if (sum(v)==0) { return("<tr><td>No enriched DBD TFs</td></tr>") }
      if (is.na(maxlines) || maxlines>sum(v)) { maxlines <- sum(v) }
      pval <- pval[v][1:maxlines]
      uc <- unname(obj$Size)[v][1:maxlines]
      ec <- unname(obj$ExpCount)[v][1:maxlines]
      gc <- unname(obj$Count)[v][1:maxlines]
      ca <- rownames(obj)[v][1:maxlines]
      df <- data.frame(Pvalue=pval, ECount=round(ec, 2), Count=gc, Size=uc)
      rownames(df) <- ca
      
      cat <- "mr"
      drive <- drive[v]      
      drive <- lapply(drive, function(x) unname(unlist(mget(x, SYMBOL))))
      drive <- lapply(drive, sort)
      drive <- lapply(drive, paste, collapse=", ")
      df$Count <- paste(sep="", '<a href="#" onclick="togglestuff2(\'d.', cat, '.', seq(along=df[,1]),
                        '\'); return false;">', df$Count, '</a><br/><span id="d.', cat, '.', seq(along=df[,1]),
                        '" class="d.', cat, '" style="font-size:0.8em;display:none;visibility:hidden;">', drive,
                          '</span>')
      
      xdf <- xtable(df, display=c("s", "e", "g", "d", "d"),
                    digits=c(NA, 3, 4, 4, 4))
      tfname <- tempfile()
      zz <- file(tfname, "w")
      sink(zz)
      print(xdf, "html")
      sink() 
      close(zz)
      foo <- readLines(tfname)
      unlink(tfname)
      foo <- foo[4:(length(foo)-1)]

      foo <- fix.xtable(foo)

      link <- c("http://dbd.mrc-lmb.cam.ac.uk/DBD/index.cgi?Search/Domain=")
      foo <- sub("(<td[^>]*>[ ]*)([^< ]+)",
                 paste(sep="", '\\1<a href="', link[1], short.organism, link[2],
                       '\\2', link[3], '"> ', '\\2 </a>'), foo)
      
      foo <- sub("<th> Count </th>",
                 paste(sep="",
                       '<th> <a href="#" onclick="togglestuff3(\'d.', cat,
                       '\');return false;"> Count </a> </th>'),
                 foo, fixed=TRUE)
      
      foo <- color.table(foo)
      paste(foo, collapse="\n")
    }
    
    tables.DBD <- h(DBD@reslist[[which]], pvalue=0.05, maxlines=NA,
                      drive=drive.DBD[[which]])
  } else {
    tables.DBD <- "<p>Not tested</p>"
  }
  
  if (!is.null(CHR)) {

    chr <- function(obj, pvalue=0.05, maxlines=NA, drive=NULL) {
      if (length(obj)==1 && is.na(obj)) {
        return("<tr><td>No enriched chromosomes</td></tr>")
      } 
      pval <- obj$Pvalue
      v <- pval <= pvalue
      if (sum(v)==0) { return("<tr><td>No enriched chromosomes</td></tr>") }
      if (is.na(maxlines) || maxlines>sum(v)) { maxlines <- sum(v) }
      pval <- pval[v][1:maxlines]
      uc <- unname(obj$Size)[v][1:maxlines]
      ec <- unname(obj$ExpCount)[v][1:maxlines]
      gc <- unname(obj$Count)[v][1:maxlines]
      ca <- rownames(obj)[v][1:maxlines]
      df <- data.frame(Pvalue=pval, ECount=round(ec, 2), Count=gc, Size=uc)
      rownames(df) <- ca
      
      cat <- "ch"
      drive <- drive[v]
      drive <- lapply(drive, function(x) unname(unlist(mget(x, SYMBOL))))
      drive <- lapply(drive, sort)
      drive <- lapply(drive, paste, collapse=", ")
      df$Count <- paste(sep="", '<a href="#" onclick="togglestuff2(\'d.', cat, '.', seq(along=df[,1]),
                          '\'); return false;">', df$Count, '</a><br/><span id="d.', cat, '.', seq(along=df[,1]),
                        '" class="d.', cat, '" style="font-size:0.8em;display:none;visibility:hidden;">', drive,
                        '</span>')
      
      xdf <- xtable(df, display=c("s", "e", "g", "d", "d"),
                    digits=c(NA, 3, 4, 4, 4))
      tfname <- tempfile()
      zz <- file(tfname, "w")
      sink(zz)
      print(xdf, "html")
      sink() 
      close(zz)
      foo <- readLines(tfname)
      unlink(tfname)
      foo <- foo[4:(length(foo)-1)]

      foo <- fix.xtable(foo)

      link <- c("", "", "")
      foo <- sub("(<td[^>]*>[ ]*)([^< ]+)",
                 paste(sep="", '\\1<a href="', link[1], short.organism, link[2],
                       '\\2', link[3], '"> ', '\\2 </a>'), foo)
      
      foo <- sub("<th> Count </th>",
                 paste(sep="",
                       '<th> <a href="#" onclick="togglestuff3(\'d.', cat,
                       '\');return false;"> Count </a> </th>'),
                 foo, fixed=TRUE)
      
      foo <- color.table(foo)
      paste(foo, collapse="\n")
    }
  
    tables.CHR <- chr(CHR@reslist[[which]], pvalue=0.05, maxlines=NA,
                      drive=drive.CHR[[which]])
  } else {
    tables.CHR <- "<p>Not tested.</p>"
  }
    
  lines.orig <- readLines(paste(sep="", template, "/module.html.in"))
  clines.orig <- readLines(paste(sep="", template, "/style-mod.css.in"))
  jlines.orig <- readLines(paste(sep="", template, "/module.js.in"))

  m <- which

  print(paste("Module", m, "expression graph"))

  ep <- expPlotCreate(nexp, getFeatureMatrix(modules, mods=m),
                      getSampleMatrix(modules, mods=m), normalize=FALSE)
  png(file=paste(sep="", target.dir, "/expression-", m, ".png"),
      width=ep$width, height=ep$height)
  ## returns the box coordinates of the expression image
  bbox <- expPlot(ep)
  bbox$coords <- cbind( bbox$coords$x * ep$width,
                        rev(ep$height - bbox$coords$y * ep$height + 1))
  dev.off() 

  ## Color bar
  png(file=paste(sep="", target.dir, "/expcolbar-", m, ".png"),
      width=840, height=56)
  expPlotColbar(ep)
  dev.off()
  
  #################################
  print(paste("Module", m, "HTML file"))
  lines <- lines.orig
  clines <- clines.orig
  jlines <- jlines.orig

  my.gth <- seedData(modules)$thr.row[m]
  my.cth <- seedData(modules)$thr.col[m]

  ## Write the coordinates to the javascript file, to have
  ## the cross

  exppos <- paste(collapse=", ",
                  as.character(c(as.vector(bbox$coords),
                                 bbox$gene.width, bbox$cond.height)))
  jlines [ grep("// exppos", jlines, fixed=TRUE)[1] ] <- exppos
  
  ## title
  print("  -- title")
  entr <- unique(unlist(mget(getFeatureNames(modules, m)[[1]], ENTREZ)))
  entr <- entr[!is.na(entr)]
  title <- paste(sep="", "Module #", m, ", TG: ", my.gth,
                 ", TC: ", my.cth, ", ", getNoFeatures(modules,m), " probes, ",
                 length(entr), " Entrez genes, ",
                 getNoSamples(modules, m), " conditions")
  lines[ grep("<!-- title -->", lines) ] <- title

  ## gth

  print("  -- gth")
  tmp <- grep("<!--gth-->", lines, fixed=TRUE)
  lines[tmp] <- sub("<!--gth-->", my.gth, lines[tmp], fixed=TRUE)

  ## no
  print("  -- module number")
  to.sub <- grep("<!--no-->", lines, fixed=TRUE)
  lines[to.sub] <- gsub("<!--no-->", as.character(m), lines[to.sub])

  to.sub <- grep("<!--prev.no-->", lines, fixed=TRUE)
  if (is.null(prev.module)) {
    prev.module <- if (m!=1) m-1 else dim(modules)[1]
  }
  lines[to.sub] <- gsub("<!--prev.no-->", as.character(prev.module), lines[to.sub])
  
  to.sub <- grep("<!--next.no-->", lines, fixed=TRUE)
  if (is.null(next.module)) {
    next.module <- if (m!=length(modules)) m+1 else 1
  }
  lines[to.sub] <- gsub("<!--next.no-->", as.character(next.module), lines[to.sub])

  to.sub <- grep("/*no*/", clines, fixed=TRUE)
  clines[to.sub] <- gsub("/*no*/", as.character(m), clines[to.sub], fixed=TRUE)

  to.sub <- grep("/*gth*/", clines, fixed=TRUE)
  clines[to.sub] <- gsub("/*gth*/", my.gth, clines[to.sub], fixed=TRUE)

  ## expx, expy
  print("  -- size of the expression plot")
  expx <- bbox$coords[2,1] - bbox$coords[1,1] + 1
  expy <- bbox$coords[2,2] - bbox$coords[1,2] + 1
  to.sub <- grep("<!--expx-->", lines, fixed=TRUE)
  lines[to.sub] <- gsub("<!--expx-->", as.character(expx), lines[to.sub])
  to.sub <- grep("<!--expy-->", lines, fixed=TRUE)
  lines[to.sub] <- gsub("<!--expy-->", as.character(expy), lines[to.sub])  

  # tableGO{BP,CC,MF}
  print("  -- enrichment tables")
  t.BP <- paste(sep="\n","<!-- tableGOBP -->", tables.BP)
  t.CC <- paste(sep="\n","<!-- tableGOCC -->", tables.CC)
  t.MF <- paste(sep="\n","<!-- tableGOMF -->", tables.MF)
  t.KEGG <- paste(sep="\n","<!-- tableKEGG -->", tables.KEGG)
  if (!is.null(miRNA)) {
    t.miRNA <- paste(sep="\n","<!-- tablemiRNA -->", tables.miRNA)
  }
  if (!is.null(DBD)) {
    t.DBD <- paste(sep="\n","<!-- tableDBD -->", tables.DBD)
  }
  if (!is.null(CHR)) {
    t.CHR <- paste(sep="\n","<!-- tableCHR -->", tables.CHR)
  }
  lines[ grep("<!-- tableGOBP -->", lines)[1] ] <- t.BP
  lines[ grep("<!-- tableGOCC -->", lines)[1] ] <- t.CC
  lines[ grep("<!-- tableGOMF -->", lines)[1] ] <- t.MF    
  lines[ grep("<!-- tableKEGG -->", lines)[1] ] <- t.KEGG
  if (!is.null(miRNA)) {
    lines[ grep("<!-- tablemiRNA -->", lines)[1] ] <- t.miRNA
  }
  if (!is.null(DBD)) {
    lines[ grep("<!-- tableDBD -->", lines)[1] ] <- t.DBD
  }
  if (!is.null(CHR)) {
    lines[ grep("<!-- tableCHR -->", lines)[1] ] <- t.CHR
  }

  # GO plots
  print("  -- Plot GO graphs")
  gp <- function(obj, pvalue=0.05, filename) {
    if (is(obj, "HyperGResult")) {
      pval <- pvalues(obj)
    } else {
      pval <- obj$Pvalue
      names(pval) <- rownames(obj)
    }
    v <- pval <= pvalue
    pval <- pval[v]
    if (length(pval) == 0) {
      require(igraph)
      g <- graph.empty()
      g$width <- 0
      g$height <- 0
      return(list(graph=g,
                  coords=list(x=numeric(), y=numeric())))
    }
    gop <- gograph(data.frame(pval),
                   colbar.length=20, label.cex=1,
                   go.terms=go.terms, GOGRAPHS=GOGRAPHS)
    png(file=filename, width=gop$width*4, height=gop$height*4)
    co <- gograph.plot(gop, coords=TRUE)
    dev.off()
    list(graph=gop, coords=co)
  }
  c.BP <- gp(GO[[1]]@reslist[[m]], 0.01,
             filename=paste(sep="", target.dir, "/go-BP-", m, ".png"))
  c.CC <- gp(GO[[2]]@reslist[[m]], 0.01,
             filename=paste(sep="", target.dir, "/go-CC-", m, ".png"))
  c.MF <- gp(GO[[3]]@reslist[[m]], 0.01,
             filename=paste(sep="", target.dir, "/go-MF-", m, ".png"))

  # Go tree annotation
  print("  -- GO graph annotation")
  ann <- function(g, cat) {
    if (vcount(g)==0) { return("") }
    no <- seq(vcount(g))
    res <- paste(sep="", '<dt><a href="http://www.godatabase.org/cgi-bin/amigo/go.cgi?view=details&amp;search_constraint=terms&amp;depth=0&amp;query=',
                 V(g)$name, '" class="location" id="location',
                 cat, no, '"></a></dt>\n',
                 '<dd><p><strong>', V(g)$desc, '</strong></p>\n<p>',
                 V(g)$definition, '</p></dd>')
    res <- paste(collapse="\n", res)
    res
  }
  a.BP <- ann(c.BP$graph, "BP")
  a.CC <- ann(c.CC$graph, "CC")
  a.MF <- ann(c.MF$graph, "MF")
  lines[ grep("<!-- GOtreeBP -->", lines)[1] ] <- a.BP
  lines[ grep("<!-- GOtreeCC -->", lines)[1] ] <- a.CC
  lines[ grep("<!-- GOtreeMF -->", lines)[1] ] <- a.MF

  # GO tree coordinates for the tooltips
  print("  -- GO graph tooltips")
  c.coords <- function(coords, width, height, cat) {
    no <- seq(length(coords$x))
    res <- paste(sep="", 'dl#map', tolower(cat), '.map.on a#location', cat,
                 no, ' { left:', floor(coords$x*width), 'px;top:',
                 height-floor(coords$y*height)-1, 'px; }')
    res <- paste(collapse="\n", res)
    res
  }
  
  co.BP <- c.coords(c.BP$coords, c.BP$graph$width*4,
                    c.BP$graph$height*4, "BP")
  co.CC <- c.coords(c.CC$coords, c.CC$graph$width*4,
                    c.CC$graph$height*4, "CC")
  co.MF <- c.coords(c.MF$coords, c.MF$graph$width*4,
                    c.MF$graph$height*4, "MF")
  clines[ grep("/* locationBP */", clines, fixed=TRUE)[1] ] <- co.BP
  clines[ grep("/* locationCC */", clines, fixed=TRUE)[1] ] <- co.CC
  clines[ grep("/* locationMF */", clines, fixed=TRUE)[1] ] <- co.MF

  clines <- gsub("/*mapbpwidth*/", as.character(floor(c.BP$graph$width*4)),
                 clines, fixed=TRUE)
  clines <- gsub("/*mapbpheight*/", as.character(floor(c.BP$graph$height*4)),
                 clines, fixed=TRUE)
  clines <- gsub("/*mapccwidth*/", as.character(floor(c.CC$graph$width*4)),
                 clines, fixed=TRUE)
  clines <- gsub("/*mapccheight*/", as.character(floor(c.CC$graph$height*4)),
                 clines, fixed=TRUE)
  clines <- gsub("/*mapmfwidth*/", as.character(floor(c.MF$graph$width*4)),
                 clines, fixed=TRUE)
  clines <- gsub("/*mapmfheight*/", as.character(floor(c.MF$graph$height*4)),
                 clines, fixed=TRUE)
  
  # Gene cloud
  print("  -- Gene cloud")
  nam <- getFeatureNames(modules, m)[[1]]
  orig.val <- getFeatureScores(modules, m)[[1]]
  val <- round(orig.val*10)
  entrezNums <- mget(nam, envir = get(paste(sep="", chip, "ENTREZID"))) 
  entrezIds <- mget(nam, envir = get(paste(sep="", chip, "SYMBOL")))
  longname <- mget(nam, envir = get(paste(sep="", chip, "GENENAME")))
  longname <- paste(sep="", longname, " (", nam, ")")
  haveEntrezId <- names(entrezIds)[sapply(entrezIds, function(x) !is.na(x))]
  
  if (! all( sapply(entrezIds, length)==1 )) {
    print("trouble");
  } else {
    entrezIds <- unlist(entrezIds)
    entrezNums <- unlist(entrezNums)
  }

  ord <- order(entrezIds)
  entrezIds <- entrezIds[ ord ]
  entrezNums <- entrezNums[ ord ]
  orig.val <- round(orig.val[ ord ], 2)
  val <- val[ ord ]
  longname <- longname[ ord ]
  
  valid <- !is.na(entrezIds)

  if (getOrganism(modules) == "Homo sapiens") {
    html <- paste(sep="", "<a href=\"http://www.genecards.org/cgi-bin/carddisp.pl?gene=", entrezIds[valid],
                  "\" class=\"tag", val[valid],
                  "\">", entrezIds[valid], "<span>", longname[valid], ", score: ", orig.val[valid], "</span></a>")
  } else {
    html <- paste(sep="", "<a href=\"http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene&cmd=Retrieve&dopt=full_report&list_uids=", entrezNums[valid],
                  "\" class=\"tag", val[valid],
                  "\">", entrezIds[valid], "<span>", longname[valid], ", score: ", orig.val[valid], "</span></a>")
  }    
  html <- paste(html, collapse="\n") 
  
  html2 <- paste(sep="", "<a href=\"", 
                 "\" class=\"tag", val[!valid],
                 "\">", nam[!valid], "<span>Unknown, score: ", orig.val[!valid], "</span></a>")
  html2 <- paste(html2, collapse="\n") 

  lines[ grep("<!-- genecloud -->", lines, fixed=TRUE)[1] ] <- html
  lines[ grep("<!-- genecloud-noname -->", lines, fixed=TRUE)[1] ] <- html2

  ## Conditions
  print("  -- conditions")

  ## rather arbitrary
  if (is.null(cond.to.include)) {
    cond.to.include <- 1:6
  }

  score <- round(getSampleMatrix(modules, mods=m), 2)
  seq <- which(score != 0)
  ord <- order(getSampleScores(modules, mods=m)[[1]], decreasing=TRUE)
  pd <- pData(modules)[seq,,drop=FALSE][ ord,,drop=FALSE]
  pd <- pd[, cond.to.include,drop=FALSE]

  ## workaround for NAs
  for (i in 1:ncol(pd)) {
    pd[[i]] <- as.character(pd[[i]])
  }

  pd <- cbind("No"=rev(seq(nrow(pd))), "Score"=score[seq][ord], pd)
  xt.pd <- xtable(pd)
  tfname <- tempfile()
  zz <- file(tfname, "w")
  sink(zz)
  print(xt.pd, "html")
  sink() 
  close(zz)
  foo <- readLines(tfname)
  unlink(tfname)
  foo <- gsub("<TR>", "<tr>", foo)
  foo <- gsub("</TR>", "</tr>", foo)
  foo <- gsub("<TD", "<td", foo)
  foo <- gsub("</TD>", "</td>", foo)
  foo <- gsub("<TH", "<th", foo)
  foo <- gsub("</TH>", "</th>", foo)
  foo <- gsub("<NA>", "NA", foo)
  foo <- foo[4:(length(foo)-1)]
  head <- foo[1]
  foo <- foo[-1]
  
  # mark under- and over-expression
  under <- score[seq][ord] < 0
  if (any(under)) {
    foo[under] <- gsub('<tr>', '<tr class="under">', foo[under])
  }
  if (!all(under)) {
    foo[!under] <- gsub('<tr>', '<tr class="over">', foo[!under])
  }
  
  # collapse  
  foo <- paste(collapse="\n", c(head, rev(foo)))
  
  lines[ grep("<!-- conditions -->", lines, fixed=TRUE) ] <- foo


  ## Create the condition plot
  
  png(file=paste(sep="", target.dir, "/condplot-", m, ".png"),
      width=1200, height=400)
  cond.plot(modules, number=m, eset=eset,
            col=cond.col, sep=sep)
  dev.off()
  
  ## Gene names for expression matrix cross
  print("  -- Gene names for cross")
  nam <- getFeatureNames(modules, m)[[1]]
  entrezIds <- mget(nam, envir = get(paste(sep="", chip, "SYMBOL")))
#  longname <- mget(nam, envir = get(paste(sep="", chip, "GENENAME")))
  haveEntrezId <- names(entrezIds)[sapply(entrezIds, function(x) !is.na(x))]
  
  if (! all( sapply(entrezIds, length)==1 )) {
    print("trouble");
  } else {
    entrezIds <- unlist(entrezIds)
  }

  ord <- order(getFeatureScores(modules, m)[[1]], decreasing=TRUE)
  nam <- nam[ ord ]
  entrezIds <- unname(entrezIds[ ord ])
#  longname <- longname[ ord ]
  valid <- !is.na(entrezIds)
  entrezIds[ !valid ] <- nam [!valid]

  to.print <- paste(sep="", collapse=",\n  ", '"', entrezIds, '"')

  jlines [ grep("// genes", jlines, fixed=TRUE)[1] ] <- to.print

  ## Conditions for cross, we still have pd
  print("  -- Condition names for cross")

  to.print <- apply(cbind(rownames(pd), pd), 1, paste, collapse=", ")
  to.print <- paste(sep="", collapse=",\n", '"', to.print, '"')
  
  jlines[ grep("// conditions", jlines, fixed=TRUE)[1] ] <- to.print
  
  ## finished, write to file
  fname <- paste(sep="", target.dir, "/module-", m, ".html")
  cat(lines, file=fname, sep="\n")

  cfname <- paste(sep="", target.dir, "/style-mod-", m, ".css")
  cat(clines, file=cfname, sep="\n")

  jfname <- paste(sep="", target.dir, "/module-", m, ".js")
  cat(jlines, file=jfname, sep="\n")

  isa2:::isa.status("DONE", "out")
}

