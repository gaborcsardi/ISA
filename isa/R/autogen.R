
autogen.table <- function(nm, isares, target.dir,
                          template=system.file("autogen", package="isa"),
                          GO=NULL, KEGG=NULL, miRNA=NULL, CHR=NULL,
                          htmltitle=NULL, notes=NULL, seed=NULL) {

  chip <- isares$rundata$annotation
  library(paste(sep="", chip, ".db"), character.only=TRUE)
  organism <- get(paste(sep="", chip, "ORGANISM"))
  short.organism <- organism

  modules <- seq_len(ncol(isares$genes))

  ################################################
  ## GO

  db <- GO_dbconn()
  query <- "SELECT go_id, term, definition FROM go_term"
  go.terms <- dbGetQuery(db, query)
  rownames(go.terms) <- go.terms[,1]
  go.terms <- go.terms[,-1]
  
  options(digits=2)
  
  f <- function(obj, pvalue=0.05) {
    
    if (class(obj)=="NULL") {
      return("")
    } else {
      if (length(obj$Pvalue)==0) { return("") }
      pval <- obj$Pvalue[1]
      if (pval > pvalue) { return("") }
      uc <- obj$Size[1]
      ec <- obj$ExpCount[1]
      gc <- obj$Count[1]
      ca <- rownames(obj)[1]
    }
    
    nn <- go.terms[ca,]$term
    paste(sep="", nn, " <span class=\"pvalue\"><br/>(", format(pval), "/", format(ec),
          "/", gc, "/", uc, ")</span>")
  }
  
  print("  -- GO BP")
  tables.BP <- lapply(GO[[1]]@reslist[modules], function(x) f(x))
  print("  -- GO CC")
  tables.CC <- lapply(GO[[2]]@reslist[modules], function(x) f(x))
  print("  -- GO MF")
  tables.MF <- lapply(GO[[3]]@reslist[modules], function(x) f(x))

  g <- function(obj, pvalue=0.05) {
    
    if (class(obj)=="NULL") {
      return("")
    } else {
      if (length(obj$Pvalue)==0) { return("") }
      pval <- obj$Pvalue[1]
      if (pval > pvalue) { return("") }
      uc <- obj$Size[1]
      ec <- obj$ExpCount[1]
      gc <- obj$Count[1]
      ca <- rownames(obj)[1]
    }

    nn <- as.character(mget(ca, KEGGPATHID2NAME))
    paste(sep="", nn, " <span class=\"pvalue\"><br/>(", format(pval), "/",
          format(ec), "/", gc, "/", uc, ")</span>")
  }
  
  require(KEGG.db)
  print("  -- KEGG")
  tables.KEGG <- sapply(KEGG@reslist[modules], g, pvalue=0.05)
  
  if (!is.null(miRNA)) {
    
    h <- function(obj, pvalue=0.05) {
      if (nrow(obj)==0) { return("") }
      pval <- obj$Pvalue[1]
      if (pval > pvalue) { return("") }
      uc <- obj$Size[1]
      ec <- obj$ExpCount[1]
      gc <- obj$Count[1]
      ca <- rownames(obj)[1]
      paste(sep="", ca, " <span class=\"pvalue\"><br/>(", format(pval), "/", format(ec),
            "/", gc, "/", uc, ")</span>")
    }
    
    print("  -- miRNA")
    tables.miRNA <- lapply(miRNA[modules], h, pvalue=0.05)
  } else {
    tables.miRNA <- ""
  }

  if (!is.null(CHR)) {
    
    chr <- function(obj, pvalue=0.05) {
      pval <- obj$Pvalue[1]
      if (pval > pvalue) { return("") }
      uc <- obj$Size[1]
      ec <- obj$ExpCount[1]
      gc <- obj$Count[1]
      ca <- rownames(obj)[1]
      paste(sep="", ca, " <span class=\"pvalue\"><br/>(", format(pval), "/", format(ec),
            "/", gc, "/", uc, ")</span>")
    }
    print("  -- CHR")
    tables.CHR <- lapply(CHR[modules], chr, pvalue=0.05)
  } else {
    tables.CHR <- ""
  }

  thr <- paste(sep="", isares$seeddata$tg, "/", isares$seeddata$tc)
  no.genes <- apply(isares$genes != 0, 2, sum)
  no.conds <- apply(isares$conditions != 0, 2, sum)

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
  if (length(tables.CHR) != 1 || tables.CHR != "") {
    tables.CHR <- paste(sep="", "<td>", tables.CHR, "</td>")
    head <- c(head, "CHR")
  }
  
  head <- paste(sep="", collapse="", "<td>", head, "</td>")
  head <- paste(collapse="", "<tr>", head, "</tr>")
  table <- paste(sep="",
                 '<tr onclick="location.href=\'module-', seq(ncol(isares$genes)), '.html\'">',
                 '<td><a href="module-', seq(ncol(isares$genes)), '.html">', seq(ncol(isares$genes)), "</a></td>",
                 seed,
                 "<td>", thr, "</td>",
                 "<td>", no.genes, "</td>",
                 "<td>", no.conds, "</td>",
                 "<td>", tables.BP, "</td>",
                 "<td>", tables.CC, "</td>",
                 "<td>", tables.MF, "</td>",
                 "<td>", tables.KEGG, "</td>",
                 tables.miRNA,
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
  
  invisible(NULL)
}  
                          

autogen.modules <- function(nm, isares, modules=seq_len(ncol(isares$genes)),
                            target.dir,
                            template=system.file("autogen", package="isa"),
                            GO=NULL, KEGG=NULL, miRNA=NULL, CHR=NULL,
                            cond.to.include=NULL,
                            markup=numeric(), markdown=numeric(),
                            sep=NULL, do.drive=FALSE, seed=NULL) {

  if (!file.exists(target.dir)) {
    dir.create(target.dir)
  }  
  if (!file.exists(paste(sep="", target.dir, "/images"))) {
    dir.create(paste(sep="", target.dir, "/images"))
  }  
  
  ## Copy all files first

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

  ## Then generate modules
  sapply(modules, function(x) {
    isa.autogen.module(nm, isares, x, target.dir=target.dir, template=template,
                       GO=GO, KEGG=KEGG, miRNA=miRNA, CHR=CHR,
                       cond.to.include=cond.to.include,
                       markup=markup, markdown=markdown, sep=sep,
                       do.drive=do.drive, seed=seed)
  })

  invisible(NULL)
}

isa.autogen.module <- function(nm, isares, module, target.dir, template,
                               GO, KEGG, miRNA, CHR, cond.to.include,
                               markup, markdown, sep=NULL, do.drive=FALSE,
                               seed=NULL) {

  require(Cairo)
  require(isa)
  require(affy)
  require(TeachingDemos)
  require(igraph)

  nexp <- nm[[2]]

  chip <- isares$rundata$annotation
  library(paste(sep="", chip, ".db"),  character.only=TRUE)
  organism <- get(paste(sep="", chip, "ORGANISM"))
  short.organism <- organism
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
    if (do.drive) {
      drive <- strsplit(as.character(obj$Drive[v]), ";", fixed=TRUE)
      drive <- lapply(drive, function(x) unname(unlist(mget(x, SYMBOL))))
      drive <- lapply(drive, sort)
      drive <- lapply(drive, paste, collapse=", ")
      df$Count <- paste(sep="", '<a href="#" onclick="togglestuff2(\'d.', cat, '.', seq(along=df[,1]),
                      '\'); return false;">', df$Count, '</a><br/><span id="d.', cat, '.', seq(along=df[,1]),
                      '" class="d.', cat, '" style="font-size:0.8em;display:none;visibility:hidden;">', drive,
                      '</span>')
    }
  
    xdf <- xtable(df, display=c("s", "e", "g", "g", "d", "s"),
                  digits=c(NA, 3, 4, 4, 4, NA))
    tfname <- tempfile()
    zz <- file(tfname, "w")
    print(xdf, type="html", file=zz)
    close(zz)
    foo <- readLines(tfname)
    unlink(tfname)
    foo <- foo[4:(length(foo)-1)]
    
    foo <- gsub("&lt ", "<", foo, fixed=TRUE)
    foo <- gsub("&gt ", ">", foo, fixed=TRUE)
    foo <- gsub("<TR>", "<tr>", foo, fixed=TRUE)
    foo <- gsub("</TR>", "</tr>", foo)
    foo <- gsub("<TD", "<td", foo)
    foo <- gsub("</TD>", "</td>", foo)
    foo <- gsub("<TH", "<th", foo)
    foo <- gsub("</TH>", "</th>", foo)  
    link <- "http://www.godatabase.org/cgi-bin/amigo/go.cgi?view=details&amp;search_constraint=terms&amp;depth=0&amp;query="
    foo <- sub("(<td[^>]*>[ ]*)(GO:[0-9]+)",
               paste(sep="", '\\1<a href="', link, '\\2"> \\2 </a>'), foo)
    
    if (do.drive) {
      foo <- sub("<th> Count </th>",
                 paste(sep="",
                       '<th> <a href="#" onclick="togglestuff3(\'d.', cat,
                       '\');return false;"> Count </a> </th>'),
                 foo, fixed=TRUE)
    }
    
    foo <- color.table(foo)
    paste(foo, collapse="\n")
  }

  tables.BP <- f(GO[[1]]@reslist[[module]], pvalue=0.05, maxlines=NA)
  tables.CC <- f(GO[[2]]@reslist[[module]], pvalue=0.05, maxlines=NA)
  tables.MF <- f(GO[[3]]@reslist[[module]], pvalue=0.05, maxlines=NA)

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
    if (do.drive) {
      drive0 <- strsplit(as.character(obj$Drive[v]), ";", fixed=TRUE)
      drive <- lapply(drive0, function(x) unname(unlist(mget(x, SYMBOL))))
      drive <- lapply(drive, sort)
      drive2 <- lapply(drive, paste, collapse=", ")
      df$Count <- paste(sep="", '<a href="#" onclick="togglestuff2(\'d.', cat, '.', seq(along=df[,1]),
                        '\'); return false;">', df$Count, '</a><br/><span id="d.', cat, '.', seq(along=df[,1]),
                        '" class="d.', cat, '" style="font-size:0.8em;display:none;visibility:hidden;">', drive2,
                        '</span>')
    }  
    
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
    
    foo <- gsub("&lt ", "<", foo, fixed=TRUE)
    foo <- gsub("&gt ", ">", foo, fixed=TRUE)
    foo <- gsub("<TR>", "<tr>", foo)
    foo <- gsub("</TR>", "</tr>", foo)
    foo <- gsub("<TD", "<td", foo)
    foo <- gsub("</TD>", "</td>", foo)
    foo <- gsub("<TH", "<th", foo)
    foo <- gsub("</TH>", "</th>", foo)
    link <- c("http://www.genome.jp/kegg-bin/mark_pathway_www?@", "/default%3dyellow/")
    foo <- sub("(<td[^>]*>[ ]*)([0-9]+)",
               paste(sep="", '\\1<a href="', link[1], keggorg,
                     '\\2', link[2], '"> \\2 </a>'), foo)
    if (do.drive) {
      ## mark genes on pathway, we still have 'drive0'
      drive2 <- sapply(drive0, paste, collapse="/")
      for (i in seq_along(foo)[-1]) {
        foo[[i]] <- sub(link[2], paste(sep="", link[2], drive2[[i-1]]),
                        foo[[i]], fixed=TRUE)
      }
    }
    
    if (do.drive) {
      foo <- sub("<th> Count </th>",
                 paste(sep="",
                       '<th> <a href="#" onclick="togglestuff3(\'d.', cat,
                       '\');return false;"> Count </a> </th>'),
                 foo, fixed=TRUE)
    }
    
    foo <- color.table(foo)
    paste(foo, collapse="\n")
  }

  require(KEGG.db)
  tables.KEGG <- g(KEGG@reslist[[module]], pvalue=0.05, maxlines=NA)

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
      if (do.drive) {
        drive0 <- strsplit(as.character(obj$Drive[v]), ";", fixed=TRUE)
        drive <- lapply(drive0, function(x) unname(unlist(mget(x, SYMBOL))))
        drive <- lapply(drive, sort)
        drive <- lapply(drive, paste, collapse=", ")
        df$Count <- paste(sep="", '<a href="#" onclick="togglestuff2(\'d.', cat, '.', seq(along=df[,1]),
                          '\'); return false;">', df$Count, '</a><br/><span id="d.', cat, '.', seq(along=df[,1]),
                          '" class="d.', cat, '" style="font-size:0.8em;display:none;visibility:hidden;">', drive,
                          '</span>')
      }
      
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
      
      foo <- gsub("&lt ", "<", foo, fixed=TRUE)
      foo <- gsub("&gt ", ">", foo, fixed=TRUE)
      foo <- gsub("<TR>", "<tr>", foo)
      foo <- gsub("</TR>", "</tr>", foo)
      foo <- gsub("<TD", "<td", foo)
      foo <- gsub("</TD>", "</td>", foo)
      foo <- gsub("<TH", "<th", foo)
      foo <- gsub("</TH>", "</th>", foo)
      link <- c("http://www.targetscan.org/cgi-bin/targetscan/vert_40/targetscan.cgi?species=", "&amp;gid=&amp;mir_c=", "&amp;mir_sc=&amp;mir_nc=&amp;mirg=")
      foo <- sub("(<td[^>]*>[ ]*)([^< ]+)",
                 paste(sep="", '\\1<a href="', link[1], short.organism, link[2],
                       '\\2', link[3], '"> ', '\\2 </a>'), foo)
      
      if (do.drive) {
        foo <- sub("<th> Count </th>",
                   paste(sep="",
                         '<th> <a href="#" onclick="togglestuff3(\'d.', cat,
                         '\');return false;"> Count </a> </th>'),
                   foo, fixed=TRUE)
      }
      
      foo <- color.table(foo)
      paste(foo, collapse="\n")
    }
    
    tables.miRNA <- h(miRNA[[module]], pvalue=0.05, maxlines=NA)
  } else {
    tables.miRNA <- "<p>Not tested</p>"
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
      if (do.drive) {
        drive <- strsplit(as.character(obj$Drive[v]), ";", fixed=TRUE)
        drive <- lapply(drive, function(x) unname(unlist(mget(x, SYMBOL))))
        drive <- lapply(drive, sort)
        drive <- lapply(drive, paste, collapse=", ")
        df$Count <- paste(sep="", '<a href="#" onclick="togglestuff2(\'d.', cat, '.', seq(along=df[,1]),
                          '\'); return false;">', df$Count, '</a><br/><span id="d.', cat, '.', seq(along=df[,1]),
                          '" class="d.', cat, '" style="font-size:0.8em;display:none;visibility:hidden;">', drive,
                          '</span>')
      }  
      
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
      
      foo <- gsub("&lt ", "<", foo, fixed=TRUE)
      foo <- gsub("&gt ", ">", foo, fixed=TRUE)
      foo <- gsub("<TR>", "<tr>", foo)
      foo <- gsub("</TR>", "</tr>", foo)
      foo <- gsub("<TD", "<td", foo)
      foo <- gsub("</TD>", "</td>", foo)
      foo <- gsub("<TH", "<th", foo)
      foo <- gsub("</TH>", "</th>", foo)
      link <- c("", "", "")
      foo <- sub("(<td[^>]*>[ ]*)([^< ]+)",
                 paste(sep="", '\\1<a href="', link[1], short.organism, link[2],
                       '\\2', link[3], '"> ', '\\2 </a>'), foo)
      
      if (do.drive) {
        foo <- sub("<th> Count </th>",
                   paste(sep="",
                         '<th> <a href="#" onclick="togglestuff3(\'d.', cat,
                         '\');return false;"> Count </a> </th>'),
                   foo, fixed=TRUE)
      }
      
      foo <- color.table(foo)
      paste(foo, collapse="\n")
    }
  
    tables.CHR <- chr(CHR[[module]], pvalue=0.05, maxlines=NA)
  } else {
    tables.CHR <- "<p>Not tested.</p>"
  }
    
  lines.orig <- readLines(paste(sep="", template, "/module.html.in"))
  clines.orig <- readLines(paste(sep="", template, "/style-mod.css.in"))
  jlines.orig <- readLines(paste(sep="", template, "/module.js.in"))

  m <- module

  print(paste("Module", m, "expression graph"))

  ep <- exp.plot.create(t(nexp), isares$genes[,m],
                        isares$conditions[,m], normalize=FALSE)
  CairoPNG(file=paste(sep="", target.dir, "/expression-", m, ".png"),
           width=ep$width, height=ep$height)
  ## returns the box coordinates of the expression image
  bbox <- exp.plot(ep)
  bbox$coords <- cbind( bbox$coords$x * ep$width,
                        rev(ep$height - bbox$coords$y * ep$height + 1))
  dev.off() 

  ## Color bar
  CairoPNG(file=paste(sep="", target.dir, "/expcolbar-", m, ".png"),
           width=840, height=56)
  exp.plot.colbar(ep)
  dev.off()
  
  #################################
  print(paste("Module", m, "HTML file"))
  lines <- lines.orig
  clines <- clines.orig
  jlines <- jlines.orig

  my.gth <- isares$seeddata$tg[m]
  my.cth <- isares$seeddata$tc[m]

  ## Write the coordinates to the javascript file, to have
  ## the cross

  exppos <- paste(collapse=", ",
                  as.character(c(as.vector(bbox$coords),
                                 bbox$gene.width, bbox$cond.height)))
  jlines [ grep("// exppos", jlines, fixed=TRUE)[1] ] <- exppos
  
  ## title
  print("  -- title")
  entr <- unique(unlist(mget(isares$rundata$features[isares$genes[,m]!=0],
                             ENTREZ)))
  entr <- entr[!is.na(entr)]
  title <- paste(sep="", "Module #", m, ", TG: ", my.gth,
                 ", TC: ", my.cth, ", ", sum(isares$genes[,m]!=0), " probes, ",
                 length(entr), " Entrez genes, ",
                 sum(isares$conditions[,m]!=0), " conditions")
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
  pno <- if (m!=1) m-1 else ncol(isares$genes)
  lines[to.sub] <- gsub("<!--prev.no-->", as.character(pno), lines[to.sub])

  to.sub <- grep("<!--next.no-->", lines, fixed=TRUE)
  pno <- if (m!=ncol(isares$genes)) m+1 else 1
  lines[to.sub] <- gsub("<!--next.no-->", as.character(pno), lines[to.sub])

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
    gop <- gograph(data.frame(names(pval), pval),
                   colbar.length=20, label.cex=1,
                   go.terms=go.terms, GOGRAPHS=GOGRAPHS)
    CairoPNG(file=filename, width=gop$width*4, height=gop$height*4)
    co <- gograph.plot(gop)
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
  seq <- which(isares$genes[,m] != 0)
  nam <- isares$rundata$features[ isares$genes[,m] != 0]
  orig.val <- isares$genes[,m] [ isares$genes[,m] != 0 ]
  val <- round(orig.val*10)
  entrezIds <- mget(nam, envir = get(paste(sep="", chip, "SYMBOL")))
  longname <- mget(nam, envir = get(paste(sep="", chip, "GENENAME")))
  longname <- paste(sep="", longname, " (", nam, ")")
  haveEntrezId <- names(entrezIds)[sapply(entrezIds, function(x) !is.na(x))]
  
  if (! all( sapply(entrezIds, length)==1 )) {
    print("trouble");
  } else {
    entrezIds <- unlist(entrezIds)
  }

  ord <- order(entrezIds)
  entrezIds <- entrezIds[ ord ]
  orig.val <- round(orig.val[ ord ], 2)
  val <- val[ ord ]
  seq <- seq[ ord ]
  longname <- longname[ ord ]
  
  valid <- !is.na(entrezIds)
  
  html <- paste(sep="", "<a href=\"http://www.genecards.org/cgi-bin/carddisp.pl?gene=", entrezIds[valid],
                "\" class=\"tag", val[valid],
                "\">", entrezIds[valid], "<span>", longname[valid], ", score: ", orig.val[valid], "</span></a>")
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

  score <- isares$conditions[,m]
  score <- round(score[ score != 0 ], 2)
  seq <- which(isares$conditions[,m] != 0)
  ord <- order(isares$conditions[seq,m], decreasing=TRUE)
  pd <- isares$rundata$pData[seq,,drop=FALSE][ ord,,drop=FALSE]
  pd <- pd[, cond.to.include,drop=FALSE]

  ## workaround for NAs
  for (i in 1:ncol(pd)) {
    pd[[i]] <- as.character(pd[[i]])
  }

  pd <- cbind("No"=rev(seq(nrow(pd))), "Score"=score[ord], pd)
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
  under <- sort(isares$conditions[seq,m], decreasing=TRUE) < 0
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
  
  CairoPNG(file=paste(sep="", target.dir, "/condplot-", m, ".png"),
           width=1200, height=400)
  cond.plot(nm, genes=isares$genes[,m], thr=my.cth,
            markup=markup, markdown=markdown, sep=sep)
  dev.off()
  
  ## Gene names for expression matrix cross
  print("  -- Gene names for cross")
  seq <- which(isares$genes[,m] != 0)
  nam <- isares$rundata$features[ isares$genes[,m] != 0]
  entrezIds <- mget(nam, envir = get(paste(sep="", chip, "SYMBOL")))
#  longname <- mget(nam, envir = get(paste(sep="", chip, "GENENAME")))
  haveEntrezId <- names(entrezIds)[sapply(entrezIds, function(x) !is.na(x))]
  
  if (! all( sapply(entrezIds, length)==1 )) {
    print("trouble");
  } else {
    entrezIds <- unlist(entrezIds)
  }

  ord <- order(isares$genes[seq,m], decreasing=TRUE)
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
}

