
ISAHTML <- function(eset, modules, target.dir,
                     template=system.file("autogen", package="eisa"),
                     GO, KEGG, miRNA=NULL, CHR=NULL, htmltitle=NULL,
                     notes=NULL, seed=NULL, table.extra=list(),
                     cond.to.include=NULL, cond.col="white", sep=NULL,
                     condPlot=TRUE, which=NULL) {

  ISAHTMLTable(modules=modules, target.dir=target.dir, template=template,
                 GO=GO, KEGG=KEGG, miRNA=miRNA, CHR=CHR, htmltitle=htmltitle,
                 notes=notes, seed=seed, extra=table.extra, which=which)

  ISAHTMLModules(eset=eset, modules=modules, target.dir=target.dir,
                 which=if (is.null(which)) which else unique(unlist(which)),
                 template=template, GO=GO, KEGG=KEGG, miRNA=miRNA, CHR=CHR,
                 cond.to.include=cond.to.include,
                 cond.col=cond.col, sep=sep, seed=seed, condPlot=condPlot)

  invisible(NULL)
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

ISAHTMLTable <- function(modules, target.dir,
                         which=NULL,
                         template=system.file("autogen", package="eisa"),
                         GO=NULL, KEGG=NULL, miRNA=NULL, CHR=NULL,
                         htmltitle=NULL, notes=NULL, seed=NULL,
                         extra=list()) {

  isa2:::isa.status("Creating HTML module table", "in")

  library(GO.db)
  library(KEGG.db)

  if (is.null(which)) {
    which <- list("All modules"=seq_len(length(modules)))
  }
  
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
    ca[ca!=""] <- switch(type,
         "GO"=go.terms$term[ match(ca[ca!=""],rownames(go.terms)) ],
         "KEGG"=as.character(mget(ca[ca!=""], KEGGPATHID2NAME)),
                 ca[ca!=""])

    res <- paste(sep="", ca, " <span class=\"pvalue\"><br/>(", format(pv), "/", ec,
                 "/", gc, "/", uc, ")</span>")
    ifelse(pv <= pvalue, res, "")
  }

  f.tables.BP <- f(GO$BP, type="GO")
  f.tables.CC <- f(GO$CC, type="GO")
  f.tables.MF <- f(GO$MF, type="GO")
  f.tables.KEGG <- f(KEGG, type="KEGG")
  f.tables.miRNA <- if (!is.null(miRNA)) f(miRNA, type="miRNA") else ""
  f.tables.CHR <- if (!is.null(CHR)) f(CHR, type="CHR") else ""

  head <- c("#", "Thr.", "#G", "#C", names(extra),
            "GO BP", "GO CC", "GO MF", "KEGG")
  
  if (!is.null(seed)) {
    head <- c(head[1], "Seed", head[2:length(head)])
    seed <- paste(sep="", "<td>", seed, "</td>")
  }

  if (!is.null(miRNA)) {
    head <- c(head, "miRNA")
  }

  if (!is.null(CHR)) {
    head <- c(head, "CHR")
  }

  head <- paste(sep="", collapse="", "<td>", head, "</td>")
  head <- paste(collapse="", "<tr>", head, "</tr>")

  ## Do all sections separately
  alltables <- character()
  for (w in seq_along(which)) {
    sec <- which[[w]]
    secname <- names(which)[w]

    tables.BP <- f.tables.BP[sec]
    tables.CC <- f.tables.CC[sec]
    tables.MF <- f.tables.MF[sec]
    tables.KEGG <- f.tables.KEGG[sec]
    tables.miRNA <- if (!is.null(miRNA)) f.tables.miRNA[sec] else "" 
    tables.CHR <- if (!is.null(CHR)) f.tables.CHR[sec] else ""
    
    thr <- paste(sep="/", featureThreshold(modules)[sec],
                 sampleThreshold(modules)[sec])
    no.genes <- getNoFeatures(modules)[sec]
    no.conds <- getNoSamples(modules)[sec]

    #############    
    
    if (!is.null(miRNA)) {
      tables.miRNA <- paste(sep="", "<td>", tables.miRNA, "</td>")
    }
    if (!is.null(CHR)) {
      tables.CHR <- paste(sep="", "<td>", tables.CHR, "</td>")
    }
    
    td.extra <- lapply(extra, function(x) {
      paste(sep="", "<td> ", x[sec], " </td>")
    })
    tables.extra <- do.call(paste, td.extra)
    
    table <- paste(sep="",
                   '<tr onclick="location.href=\'module-', sec, '.html\'">',
                   '<td><a href="module-', sec, '.html">', sec, "</a></td>",
                   seed[sec],
                   "<td>", thr, "</td>",
                   "<td>", no.genes, "</td>",
                   "<td>", no.conds, "</td>",
                   tables.extra,
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

    secheader <- paste(sep="", "<a name=\"", gsub("\\W", "", secname),
                       "\"></a>", "<h2>", secname, "</h2>")
    alltables <- c(alltables, secheader, table)
  }
  
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
  lines[ grep("<!-- table -->", lines) ] <-
    paste(alltables, collapse="\n")
  
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
                          

ISAHTMLModules <- function(eset, modules, which=NULL,
                             target.dir,
                             template=system.file("autogen", package="eisa"),
                             GO=NULL, KEGG=NULL, miRNA=NULL, CHR=NULL,
                             cond.to.include=NULL,
                             cond.col="white",
                             sep=NULL, seed=NULL, condPlot=TRUE) {

  isa2:::isa.status("Generating module pages", "in")

  library(GO.db)
  library(KEGG.db)
  library(igraph)
  library(xtable)

  if (is.null(which)) { which <- seq_len(length(modules)) }
  
  chip <- annotation(modules)
  require(paste(sep="", chip, ".db"),  character.only=TRUE)
  organism <- getOrganism(modules)
  require(paste(sep="", "org.", abbreviate(organism, 2), ".eg.db"),
          character.only=TRUE)
  
  isa.autogen.create.dirs(template, target.dir)

  drive.BP <- drive.CC <- drive.MF <- drive.KEGG <- drive.miRNA <-
    drive.CHR <- NULL
  if (!is.null(GO)) drive.BP <- geneIdsByCategory(GO$BP)
  if (!is.null(GO)) drive.CC <- geneIdsByCategory(GO$CC)
  if (!is.null(GO)) drive.MF <- geneIdsByCategory(GO$MF)
  if (!is.null(KEGG)) drive.KEGG <- geneIdsByCategory(KEGG)
  if (!is.null(miRNA)) drive.miRNA <- geneIdsByCategory(miRNA) 
  if (!is.null(CHR)) drive.CHR <- geneIdsByCategory(CHR) 

  ## Read the templates only once
  lines.orig <- readLines(paste(sep="", template, "/module.html.in"))
  clines.orig <- readLines(paste(sep="", template, "/style-mod.css.in"))
  jlines.orig <- readLines(paste(sep="", template, "/module.js.in"))  

  ## Normalize eset only once
  eset <- eisa.get.nm(eset, modules)

  ## go.terms only once
  db <- GO_dbconn()
  query <- "SELECT go_id, term, definition FROM go_term"
  go.terms <- dbGetQuery(db, query)
  rownames(go.terms) <- go.terms[,1]
  go.terms <- go.terms[,-1]  

  ## create GO graphs only once
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

  SYMBOL <- toTable(get( paste(sep="", "org.", abbreviate(organism, 2), ".egSYMBOL")))
  ENTREZ <- toTable(get( paste(sep="", chip, "ENTREZID") ))
  CHIPSYMBOL <- toTable(get( paste(sep="", chip, "SYMBOL") ))
  GENENAME <- toTable(get( paste(sep="", chip, "GENENAME") ))
  
  ## Then generate modules
  for (i in seq_along(which)) {
    x <- which[i]
    nx <- if (i!=length(which)) which[i+1] else which[1]
    px <- if (i!=1) which[i-1] else which[length(which)]
    isa.autogen.module(eset, modules, x,
                       target.dir=target.dir, template=template,
                       GO=GO, KEGG=KEGG, miRNA=miRNA, CHR=CHR,
                       cond.to.include=cond.to.include,
                       cond.col=cond.col, sep=sep, condPlot=condPlot,
                       seed=seed, drive.BP=drive.BP, drive.CC=drive.CC,
                       drive.MF=drive.MF, drive.KEGG=drive.KEGG,
                       drive.miRNA=drive.miRNA, drive.CHR=drive.CHR,
                       next.module=nx, prev.module=px,
                       lines=lines.orig, clines=clines.orig, jlines=jlines.orig,
                       go.terms=go.terms, GOGRAPHS=GOGRAPHS,
                       SYMBOL=SYMBOL, ENTREZ=ENTREZ, CHIPSYMBOL=CHIPSYMBOL,
                       GENENAME=GENENAME)
  }

  isa2:::isa.status("DONE", "out")
  
  invisible(NULL)
}

isa.autogen.module <- function(eset, modules, which, target.dir, template,
                               GO, KEGG, miRNA, CHR, cond.to.include,
                               cond.col="white", sep=NULL, condPlot=TRUE,
                               seed=NULL, drive.BP=NULL, drive.CC=NULL,
                               drive.MF=NULL, drive.KEGG=NULL,
                               drive.miRNA=NULL, drive.CHR=NULL,
                               next.module=NULL, prev.module=NULL,
                               lines, clines, jlines, go.terms, GOGRAPHS,
                               SYMBOL, ENTREZ, CHIPSYMBOL, GENENAME) {

  isa2:::isa.status(paste("Generating HTML page for module", which), "in")

  organism <- getOrganism(modules)

  if (is.null(drive.BP) && !is.null(GO)) drive.BP <- geneIdsByCategory(GO$BP)
  if (is.null(drive.CC) && !is.null(GO)) drive.CC <- geneIdsByCategory(GO$CC)
  if (is.null(drive.MF) && !is.null(GO)) drive.MF <- geneIdsByCategory(GO$MF)
  if (is.null(drive.KEGG) && !is.null(KEGG)) drive.KEGG <- geneIdsByCategory(KEGG)
  if (is.null(drive.miRNA) && !is.null(miRNA)) drive.miRNA <-
    geneIdsByCategory(miRNA) 
  if (is.null(drive.CHR) && !is.null(CHR)) drive.CHR <- geneIdsByCategory(CHR) 
  
  color.table <- function(t) {
    ## colorify every second row, plus the first one
    t[1] <- sub('<tr>', '<tr class="head">', t[1])
    if (length(t) > 2) {
      idx <- seq(3, length(t), by=2)
      t[ idx ] <- sub('<tr>', '<tr class="even">', t[ idx ])
    }
    t
  }

  tabulate <- function(df, drive, pvalue, type, link=NA) {
    df <- df[ df$Pvalue <= pvalue, ]

    ## Do we have anything left?
    if (nrow(df)==0) { return("No enriched terms") }
    
    ## Remove unwanted columns, add a term column
    df <- df[, ! colnames(df) %in% c("drive", "OddsRatio")]
    extra.display=character()
    if (type %in% c("BP", "CC", "MF")) {
      df$Term <- go.terms$term[ match(rownames(df), rownames(go.terms)) ]
      extra.display=c(extra.display, "s")
    }
    if (type=="ke") {
      df$Term <- as.character(mget(rownames(df), KEGGPATHID2NAME))
      extra.display=c(extra.display, "s")
    }

    ## Add the driving genes
    drive <- drive[ seq_len(nrow(df)) ]
    drive1 <- lapply(drive, function(x) SYMBOL$symbol[ match(x,SYMBOL$gene_id)])
    drive1 <- lapply(drive1, sort)
    drive1 <- lapply(drive1, paste, collapse=", ")
    df$Count <- paste(sep="", '<a href="#" onclick="togglestuff2(\'d.', type, '.',
                      seq(along=df[,1]), '\'); return false;">', df$Count,
                      '</a><br/><span id="d.', type, '.', seq(along=df[,1]),
                      '" class="d.', type,
                      '" style="font-size:0.8em;display:none;visibility:hidden;">',
                      drive1, '</span>')

    ## Add links
    orig.link <- link
    if (!is.na(link)) {
      link <- rep(link, nrow(df))
      if (grepl("<rn>", orig.link, fixed=TRUE)) {
        link <- sapply(seq_len(nrow(df)), function(x)
                       sub("<rn>", rownames(df)[x], link[x], fixed=TRUE))
      }
      if (grepl("<kegg>", orig.link, fixed=TRUE)) {
        kegg <- sapply(drive, paste, collapse="/")
        link <- sapply(seq_along(kegg), function(x)
                       sub("<kegg>", kegg[x], link[x], fixed=TRUE))
      }
    }
    
    ## Create HTML
    foo <- html.df(df, link=link, display=c("s", "s", "e", "g", "g", "d", extra.display),
                   digits=c(NA, NA, 3, 4, 4, 4, rep(NA, length(extra.display))))
    foo <- foo[4:(length(foo)-1)]
    foo <- color.table(foo)
    
    foo <- sub("<th> Count </th>",
               paste(sep="",
                     '<th> <a href="#" onclick="togglestuff3(\'d.', type,
                     '\');return false;"> Count </a> </th>'),
               foo, fixed=TRUE)
    
    paste(foo, collapse="\n")
  }

  link <- "http://www.godatabase.org/cgi-bin/amigo/go.cgi?view=details&amp;search_constraint=terms&amp;depth=0&amp;query=<rn>"
  tables.BP <- tabulate(GO[[1]]@reslist[[which]], pvalue=0.05, link=link,
                        drive=drive.BP[[which]], type="BP")
  tables.CC <- tabulate(GO[[2]]@reslist[[which]], pvalue=0.05, link=link,
                        drive=drive.CC[[which]], type="CC")
  tables.MF <- tabulate(GO[[3]]@reslist[[which]], pvalue=0.05, link=link,
                        drive=drive.MF[[which]], type="MF")

  keggorg <- switch(organism, "Homo sapiens"="hsa",
                    "Mus musculus"="mmu", "map")
  link <- paste(sep="", "http://www.genome.jp/kegg-bin/mark_pathway_www?@",
                keggorg, "<rn>/default%3dyellow/<kegg>")
  tables.KEGG <- tabulate(KEGG@reslist[[which]], pvalue=0.05, link=link,
                          drive=drive.KEGG[[which]], type="ke")
  
  if (!is.null(miRNA)) {
    miRNA.org <- switch(organism, "Homo sapiens"="Human",
                        "Mus musculus"="Mouse")
    link <- paste(sep="", "http://www.targetscan.org/cgi-bin/targetscan/",
                  "vert_50/targetscan.cgi?species=", miRNA.org, ";mir_c=<rn>")
    tables.miRNA <- tabulate(miRNA@reslist[[which]], pvalue=0.05, link=link,
                             drive=drive.miRNA[[which]], type="mr")
  } else {
    tables.miRNA <- "<p>Not tested</p>"
  }

  if (!is.null(CHR)) {
    tables.CHR <- tabulate(CHR@reslist[[which]], pvalue=0.05, link=NA,
                           drive=drive.CHR[[which]], type="ch")
  } else {
    tables.CHR <- "<p>Not tested.</p>"
  }
    
  m <- which

  ep <- expPlotCreate(eset, modules, m, norm="sample")
  png(filename=paste(sep="", target.dir, "/expression-", m, ".png"),
      width=ep$width, height=ep$height)
  ## returns the box coordinates of the expression image
  bbox <- expPlot(ep)
  bbox$coords <- cbind( bbox$coords$x * ep$width,
                        rev(ep$height - bbox$coords$y * ep$height + 1))
  dev.off() 

  ## Color bar
  png(filename=paste(sep="", target.dir, "/expcolbar-", m, ".png"),
      width=840, height=56)
  expPlotColbar(ep)
  dev.off()
  
  #################################

  my.gth <- seedData(modules)$thr.row[m]
  my.cth <- seedData(modules)$thr.col[m]
  if (is.null(my.gth)) { my.gth <- NA }
  if (is.null(my.cth)) { my.cth <- NA }

  ## Write the coordinates to the javascript file, to have
  ## the cross

  exppos <- paste(collapse=", ",
                  as.character(c(as.vector(bbox$coords),
                                 bbox$gene.width, bbox$cond.height)))
  jlines [ grep("// exppos", jlines, fixed=TRUE)[1] ] <- exppos
  
  ## title
  entr <- ENTREZ$gene_id[ match(getFeatureNames(modules, m)[[1]], ENTREZ$probe_id) ]
  entr <- entr[!is.na(entr)]
  title <- paste(sep="", "Module #", m, ", TG: ", my.gth,
                 ", TC: ", my.cth, ", ", getNoFeatures(modules,m), " probes, ",
                 length(entr), " Entrez genes, ",
                 getNoSamples(modules, m), " conditions")
  lines[ grep("<!-- title -->", lines) ] <- title

  ## gth

  tmp <- grep("<!--gth-->", lines, fixed=TRUE)
  lines[tmp] <- sub("<!--gth-->", my.gth, lines[tmp], fixed=TRUE)

  ## no
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
  expx <- bbox$coords[2,1] - bbox$coords[1,1] + 1
  expy <- bbox$coords[2,2] - bbox$coords[1,2] + 1
  to.sub <- grep("<!--expx-->", lines, fixed=TRUE)
  lines[to.sub] <- gsub("<!--expx-->", as.character(expx), lines[to.sub])
  to.sub <- grep("<!--expy-->", lines, fixed=TRUE)
  lines[to.sub] <- gsub("<!--expy-->", as.character(expy), lines[to.sub])  

  # tableGO{BP,CC,MF}
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
      g <- graph.empty()
      g$width <- 0
      g$height <- 0
      return(list(graph=g,
                  coords=list(x=numeric(), y=numeric())))
    }
    gop <- gograph(data.frame(pval),
                   colbar.length=20, label.cex=1,
                   go.terms=go.terms, GOGRAPHS=GOGRAPHS)
    png(filename=filename, width=gop$width*4, height=gop$height*4)
    co <- gographPlot(gop, coords=TRUE)
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
  nam <- getFeatureNames(modules, m)[[1]]
  orig.val <- getFeatureScores(modules, m)[[1]]
  val <- round(orig.val*10)
  entrezNums <- ENTREZ$gene_id[ match(nam, ENTREZ$probe_id) ]
  entrezIds <- CHIPSYMBOL$symbol[ match(nam, CHIPSYMBOL$probe_id) ]
  longname <- GENENAME$gene_name[ match(nam, GENENAME$probe_id) ]
  longname <- paste(sep="", longname, " (", nam, ")")
  haveEntrezId <- names(entrezIds)[sapply(entrezIds, function(x) !is.na(x))]
  
  if (! all( sapply(entrezIds, length)==1 )) {
    stop("A probe maps to more than one Entrez genes")
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

  if (organism == "Homo sapiens") {
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

  ## rather arbitrary
  if (is.null(cond.to.include)) {
    if (ncol(pData(modules)) < 6) {
      cond.to.include <- seq_len(ncol(pData(modules)))
    } else { 
      cond.to.include <- 1:6
    }
  }

  score <- getSampleMatrix(modules, mods=m)
  seq <- which(score != 0)
  score <- round(score, 2)
  ord <- order(getSampleScores(modules, mods=m)[[1]], decreasing=TRUE)
  pd <- pData(modules)[seq,,drop=FALSE][ ord,,drop=FALSE]
  pd <- pd[, cond.to.include,drop=FALSE]

  ## workaround for NAs
  for (i in seq_len(ncol(pd))) {
    pd[[i]] <- as.character(pd[[i]])
  }

  foo <- html.df(pd)
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

  if (condPlot) {
    png(filename=paste(sep="", target.dir, "/condplot-", m, ".png"),
        width=1200, height=400)
    condPlot(modules, number=m, eset=eset,
             col=cond.col, sep=sep)
    dev.off()
  }
  
  ## Gene names for expression matrix cross
  nam <- getFeatureNames(modules, m)[[1]]
  entrezIds <- CHIPSYMBOL$symbol[ match(nam, CHIPSYMBOL$probe_id) ]
#  longname <- mget(nam, envir = get(paste(sep="", chip, "GENENAME")))
  haveEntrezId <- names(entrezIds)[sapply(entrezIds, function(x) !is.na(x))]
  
  entrezIds <- unlist(entrezIds)

  ord <- order(getFeatureScores(modules, m)[[1]], decreasing=TRUE)
  nam <- nam[ ord ]
  entrezIds <- unname(entrezIds[ ord ])
#  longname <- longname[ ord ]
  valid <- !is.na(entrezIds)
  entrezIds[ !valid ] <- nam [!valid]

  to.print <- paste(sep="", collapse=",\n  ", '"', entrezIds, '"')

  jlines [ grep("// genes", jlines, fixed=TRUE)[1] ] <- to.print

  ## Conditions for cross, we still have pd
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

