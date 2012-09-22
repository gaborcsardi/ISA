
gograph <- function(table, colbar.length=30, label.cex=1, alpha=1, abbrev=5,
                    GOGRAPHS=NULL, go.terms=NULL) {

  isa2:::isa.status("Creating a GO graph", "in")
  
  library(GO.db)
  library(igraph0)
  
  terms <- rownames(table)
  pval <- table[,1]

  #######################
  # Check which GO tree
  
  cats <- get(terms[1], GOTERM)@Ontology

  #######################
  # Create a list of terms
  
  if (is.null(go.terms)) {
    db <- GO_dbconn()
    query <- paste("SELECT go_id, term, definition FROM go_term ",
                   "WHERE ontology=='", cats, "'",
                   sep="")
    go.terms <- dbGetQuery(db, query)
    rownames(go.terms) <- go.terms[,1]
    go.terms <- go.terms[,-1]
  }
    
  #######################
  # Create the igraph object if it was not passed as an argument
  
  if (is.null(GOGRAPHS)) {
    query <- paste("SELECT id1.go_id, id2.go_id, par.relationship_type FROM",
                   " go_", tolower(cats), "_parents AS par,",
                   " go_term AS id1, go_term AS id2",
                   " WHERE par._id==id1._id AND par._parent_id==id2._id",
                   sep="")
    db <- GO_dbconn()
    parents <- dbGetQuery(db, query)
    colnames(parents) <- c("from", "to", "relationship")
    go.graph <- graph.data.frame(parents, directed=TRUE)
  } else {
    go.graph <- switch(cats, CC=GOGRAPHS$CC, BP=GOGRAPHS$BP, MF=GOGRAPHS$MF)
  }
  
  #######################
  # Create a subgraph containing all up-paths from the
  # given vertices

  vv <- which( V(go.graph)$name %in% terms )-1
  root <- which(igraph0::degree(go.graph, 1:vcount(go.graph)-1, mode="out")==0)-1
  
  vert <- numeric()
  for (i in vv) {
    vert <- c(vert, unlist(neighborhood(go.graph, vcount(go.graph), i, mode="out")))
  }
  vert <- unique(vert)

  g <- subgraph(go.graph, vert)

  #######################
  # P values for the graph
  
  ppval <- rep(NA, vcount(g))
  for (i in 1:vcount(g)) {
    ppval[ which(terms[i] == V(g)$name) ] <- pval[i]
  }
  pval.label <- round(-log10(ppval))
  color.idx <- ifelse(is.na(pval.label), 1, pval.label+1)

  #######################
  # Create color bar
  
  colbar <- hcl(h=260, c=35, l=seq(30, 100, length=colbar.length), alpha=alpha)
  colbar <- c("#FFFFFF", rev(colbar))
  if (length(colbar) < max(color.idx)) {
    colbar <- c(colbar, rep(colbar[length(colbar)],
                            max(color.idx)+1-length(colbar)))
  }

  V(g)$color <- colbar[ color.idx ]
  V(g)$pval <- ppval
  V(g)$plabel <- pval.label
  
  #######################
  ## Prepare for plotting, unfold into a tree 
  
  r2 <- tail(topological.sort(g), 1)
  g2 <- unfold.tree(g, roots=r2, mode="in")
  index <- g2[[2]]
  g2 <- g2[[1]]

  V(g2)$color <- V(g)$color[ index+1 ]
  V(g2)$name <- V(g)$name[ index+1 ]
  V(g2)$plabel <- V(g)$plabel[ index+1 ]
  V(g2)$label <- index+1
  E(g2)$type <- E(g)$relationship               # edge ids are the same
  E(g2)$color <- ifelse( E(g2)$type=="isa", "lightblue", "darkgrey")

  freq <- unname(table(index)[ as.character(index) ])
  V(g2)$label <- ifelse(freq > 1, paste(sep="", V(g2)$label, ",", freq), "")
  
  l <- layout.reingold.tilford(g2, mode="all", root=r2)
  l <- cbind(l[,2], l[,1])

  #######################
  ## Abbreviate GO terms

  go.terms.sub <- go.terms[ V(g2)$name, ]
  definition <- go.terms.sub$definition
  desc <- go.terms.sub$term
  abbrv <- unname(abbreviate(desc, minlength=abbrev))
  if (any(nchar(abbrv) > 10)) {
    abbrv <- abbreviate(abbrv, method="both.sides", minlength=abbrev)
  }

  V(g2)$desc <- desc
  V(g2)$abbrv <- abbrv
  V(g2)$definition <- definition
  
  #######################
  ## Predict ideal size, a bit arbitrary
  
  char.width <- 1.82
  char.height <- 2.55
  rec.width <- 8 * char.width
  rec.height <- char.width + char.height
  gap.height <- rec.height / 4
  arr.width <- 8 * char.width
  
  nodes.x <- range(l[,1])
  nodes.x <- nodes.x[2] - nodes.x[1] + 1
  width <- nodes.x * (rec.width + arr.width)
  l[,1] <- (l[,1]-min(l[,1])) * (rec.width+arr.width) + (rec.width+arr.width)/2

  ## nonzero is here to ignore an igraph bug in the
  ## Reingold-Tilford implementation, sometime two vertices
  ## are put at the very same coordinates
  nonzero <- function(x) x[ abs(x) > 1e-12 ]
  nodes.y <- tapply(l[,2], l[,1], function(x) {
    if (length(x)==1) Inf else min(nonzero(diff(sort(x)))) })
  nodes.y <- min(nodes.y)
  if (nodes.y==Inf) {
    l[,2] <- l[,2] + (gap.height+rec.height)/2
    height <- rec.height + 2 * gap.height
  } else {
    l[,2] <- (l[,2]-min(l[,2]))/nodes.y * (gap.height+rec.height) +
      (gap.height+rec.height)/2
    height <- range(l[,2])
    height[1] <- height[1] - rec.height/2 - gap.height
    height[2] <- height[2] + rec.height/2 + gap.height
    height <- height[2] - height[1]
  }

  g2$width <- width
  g2$height <- height

  #######################
  ## Other paratemeters

  g2$layout <- l
  V(g2)$size <- rec.width/2
  V(g2)$size2 <- rec.height/2
  E(g2)$arrow.size <- 0.5
  V(g2)$shape <- "vrectangle"
  V(g2)$label.color <- "black"
  V(g2)$label.cex <- label.cex
  V(g2)$frame.color <- "grey"

  isa2:::isa.status("DONE", "out")
  
  g2
}

## This is from the TeachingDemos package, by Greg Snow. Thanks!

"cnvrt.coords" <-
function(x,y=NULL,input=c('usr','plt','fig','dev','tdev')) {

  input <- match.arg(input)
  xy <- xy.coords(x,y, recycle=TRUE)

  cusr <- par('usr')
  cplt <- par('plt')
  cfig <- par('fig')
  cdin <- par('din')
  comi <- par('omi')
  cdev <- c(comi[2]/cdin[1],(cdin[1]-comi[4])/cdin[1],
            comi[1]/cdin[2],(cdin[2]-comi[3])/cdin[2])

  if(input=='usr'){
    usr <- xy

    plt <- list()
    plt$x <- (xy$x-cusr[1])/(cusr[2]-cusr[1])
    plt$y <- (xy$y-cusr[3])/(cusr[4]-cusr[3])

    fig <- list()
    fig$x <- plt$x*(cplt[2]-cplt[1])+cplt[1]
    fig$y <- plt$y*(cplt[4]-cplt[3])+cplt[3]

    dev <- list()
    dev$x <- fig$x*(cfig[2]-cfig[1])+cfig[1]
    dev$y <- fig$y*(cfig[4]-cfig[3])+cfig[3]

    tdev <- list()
    tdev$x <- dev$x*(cdev[2]-cdev[1])+cdev[1]
    tdev$y <- dev$y*(cdev[4]-cdev[3])+cdev[3]

    return( list( usr=usr, plt=plt, fig=fig, dev=dev, tdev=tdev ) )
  }

  if(input=='plt') {

    plt <- xy

    usr <- list()
    usr$x <- plt$x*(cusr[2]-cusr[1])+cusr[1]
    usr$y <- plt$y*(cusr[4]-cusr[3])+cusr[3]

    fig <- list()
    fig$x <- plt$x*(cplt[2]-cplt[1])+cplt[1]
    fig$y <- plt$y*(cplt[4]-cplt[3])+cplt[3]

    dev <- list()
    dev$x <- fig$x*(cfig[2]-cfig[1])+cfig[1]
    dev$y <- fig$y*(cfig[4]-cfig[3])+cfig[3]

    tdev <- list()
    tdev$x <- dev$x*(cdev[2]-cdev[1])+cdev[1]
    tdev$y <- dev$y*(cdev[4]-cdev[3])+cdev[3]

    return( list( usr=usr, plt=plt, fig=fig, dev=dev, tdev=tdev ) )
  }

  if(input=='fig') {

    fig <- xy

    plt <- list()
    plt$x <- (fig$x-cplt[1])/(cplt[2]-cplt[1])
    plt$y <- (fig$y-cplt[3])/(cplt[4]-cplt[3])

    usr <- list()
    usr$x <- plt$x*(cusr[2]-cusr[1])+cusr[1]
    usr$y <- plt$y*(cusr[4]-cusr[3])+cusr[3]

    dev <- list()
    dev$x <- fig$x*(cfig[2]-cfig[1])+cfig[1]
    dev$y <- fig$y*(cfig[4]-cfig[3])+cfig[3]

    tdev <- list()
    tdev$x <- dev$x*(cdev[2]-cdev[1])+cdev[1]
    tdev$y <- dev$y*(cdev[4]-cdev[3])+cdev[3]

    return( list( usr=usr, plt=plt, fig=fig, dev=dev, tdev=tdev ) )
  }

  if(input=='dev'){
    dev <- xy

    fig <- list()
    fig$x <- (dev$x-cfig[1])/(cfig[2]-cfig[1])
    fig$y <- (dev$y-cfig[3])/(cfig[4]-cfig[3])

    plt <- list()
    plt$x <- (fig$x-cplt[1])/(cplt[2]-cplt[1])
    plt$y <- (fig$y-cplt[3])/(cplt[4]-cplt[3])

    usr <- list()
    usr$x <- plt$x*(cusr[2]-cusr[1])+cusr[1]
    usr$y <- plt$y*(cusr[4]-cusr[3])+cusr[3]

    tdev <- list()
    tdev$x <- dev$x*(cdev[2]-cdev[1])+cdev[1]
    tdev$y <- dev$y*(cdev[4]-cdev[3])+cdev[3]

    return( list( usr=usr, plt=plt, fig=fig, dev=dev, tdev=tdev ) )
  }

  if(input=='tdev'){
    tdev <- xy

    dev <- list()
    dev$x <- (tdev$x-cdev[1])/(cdev[2]-cdev[1])
    dev$y <- (tdev$y-cdev[3])/(cdev[4]-cdev[3])

    fig <- list()
    fig$x <- (dev$x-cfig[1])/(cfig[2]-cfig[1])
    fig$y <- (dev$y-cfig[3])/(cfig[4]-cfig[3])

    plt <- list()
    plt$x <- (fig$x-cplt[1])/(cplt[2]-cplt[1])
    plt$y <- (fig$y-cplt[3])/(cplt[4]-cplt[3])

    usr <- list()
    usr$x <- plt$x*(cusr[2]-cusr[1])+cusr[1]
    usr$y <- plt$y*(cusr[4]-cusr[3])+cusr[3]

    tdev <- list()
    tdev$x <- dev$x*(cdev[2]-cdev[1])+cdev[1]
    tdev$y <- dev$y*(cdev[4]-cdev[3])+cdev[3]

    return( list( usr=usr, plt=plt, fig=fig, dev=dev, tdev=tdev ) )
  }

}

gographPlot <- function(graph, coords=FALSE, ...) {

  if (dev.cur() == 1) {
    device <- options("device")[[1]]
    do.call( device, list(width=graph$width/15, height=graph$height/15))
  }
  par(mar=c(0,0,0,0))
  plot(0, type="n", xlim=c(0,graph$width),
       ylim=c(0,graph$height), axes=FALSE)
  par(xpd=TRUE)
  plot(graph, add=TRUE, asp=FALSE, rescale=FALSE, vertex.label=V(graph)$abbrv,
       vertex.size=V(graph)$size*200, vertex.size2=V(graph)$size2*200, ...)

  text(graph$layout[,1]-V(graph)$size, graph$layout[,2], V(graph)$plabel,
       pos=2, cex=1, col="blue", font=2)
  text(graph$layout[,1]+V(graph)$size, graph$layout[,2], V(graph)$label,
       pos=4, cex=1, col="darkgreen", family="mono")

  if (coords) {
    cnvrt.coords(graph$layout[,1]-V(graph)$size,
                 graph$layout[,2]+V(graph)$size2)$tdev
  } else {
    invisible(NULL)
  }
}

expPlotCreate <- function(eset, modules, which,
                          norm=c("sample", "raw", "feature")) {

  isa2:::isa.status("Creating an expression plot", "in")

  if (length(which) != 1 || which < 1 || which > length(modules)) {
    stop("Invalid `which' argument, should be a single method")
  }

  norm <- match.arg(norm)
  
  exp.matrix <- select.eset(eset, modules, norm)
  genes <- getFeatureMatrix(modules, mods=which)
  conditions <- getSampleMatrix(modules, mods=which)
  
  gg <- which(genes != 0)
  cc <- which(conditions != 0)
  g.order <- order(genes[gg], decreasing=TRUE)
  c.order <- order(conditions[cc], decreasing=TRUE)
  em <- exp.matrix [ gg[g.order], cc[c.order], drop=FALSE ]
  
  ## proper color bar
  
  r <- range(em)
  zlim <- c( -max(abs(r)), max(abs(r)) )
  col.green <- hcl(h=120, c=50, l=seq(20,80,length=30))
  col.red <- hcl(h=0, c=50, l=seq(20,80,length=30))
  colbar <- c( col.green, "#ffffff", rev(col.red))

  ## Determine layout, the expression plot should be around
  ## 1000 pixels wide, but one gene should be at most 15 pixels
  ## We keep 50 pixels for the axes and 100 for the condition score
  ## plot
  gene.width.px <- max(min(15, round(900 / length(gg))), 1)

  ## Conditions are at most 15 pix height, and at least 1.
  ## The plot should be about 500 px height, 50 is kept for the
  ## axes, 50 for the gene scores
  cond.height.px <- max(min(15, round(400 / length(cc))), 1)

  exp.width <- gene.width.px * length(gg)
  exp.height <- cond.height.px * length(cc)
  full.width <- 50 + exp.width + 100
  full.height <- 50 + exp.height + 70

  isa2:::isa.status("DONE", "out")
  
  res <- list(exp=em, width=full.width, height=full.height,
              exp.width=exp.width, exp.height=exp.height, colbar=colbar,
              zlim=zlim, gene.score=genes[gg][g.order],
              cond.score=conditions[cc][c.order],
              gene.width.px=gene.width.px, cond.height.px=cond.height.px)
  class(res) <- "ISAexpPlot"
  res
}

print.ISAexpPlot <- function(x, ...) {
  cat("An expression plot for an ISA module, use 'expPlot' to plot it.\n")
}

expPlotColbar <- function(epo) {
  par(mar=c(1,1,2,1))
  image( matrix((-30):30, nc=1), col=epo$colbar, axes=FALSE)
  at <- seq(0, 60, length=13)/60
  label <- round(seq(epo$zlim[1], epo$zlim[2], length=13), 2)
  axis(3, at=at, label=label, tick=FALSE, line=-1, cex.axis=1.5)
  abline(v=(0:61-0.5)/60)
  abline(h=c(-1,1))
  invisible(NULL)
}

expPlot <- function(epo, scores=TRUE) {

  if (scores) {
  
    layout( matrix(c(5,2,4, 6,1,3, 7,8,9), nc=3, byrow=TRUE),
           c( 50/epo$width, epo$exp.width/epo$width, 100/epo$width ),
           c( 70/epo$height, epo$exp.height/epo$height, 50/epo$height) )

  }
  
  no.genes <- nrow(epo$exp)
  no.conds <- ncol(epo$exp)

  par(cex.lab=1.5, mar=c(0,0,0,0), las=1)
  image(epo$exp, col=epo$colbar, axes=FALSE, asp=FALSE,
        xlab="genes", ylab=NA, zlim=epo$zlim, cex.lab=2,
        xaxs="i", yaxs="i")
  title(ylab="conditions", line=4, cex.lab=2)
  label1 <- unique(round(seq(1, no.genes, length=10)))
  at1 <- (label1-1) / (no.genes-1)
  if (length(at1) != 1) {
    axis(1, at=at1, label=label1, cex.axis=2)
  }
  if (epo$cond.height.px >= 10) {
    label2 <- seq(1, no.conds, by=2)
  } else {
    label2 <- unique(round(seq(1, no.conds, length=5)))
  }
  at2 <- (no.conds-label2) / (no.conds-1)
  if (any(is.finite(at2))) {
    axis(2, at=at2, label=label2, cex.axis=2, tick=FALSE)
  }

  usr <- par("usr")
  bbox <- cnvrt.coords(x=usr[1:2], y=usr[3:4])$tdev
  
  #########
  
  if (scores) {  
    pp <- 1/(no.genes-1)/2
    if (!is.finite(pp)) pp <- 0.5
    
    par(mar=c(1,0,1,0))
    image(matrix(c(0,0,0,0), nr=1, nc=1), zlim=epo$zlim,
          col="#00000000", axes=FALSE, xaxs="i", yaxs="i",
          xlab=NA, ylab=NA, xlim=c(-pp,1+pp), ylim=c(-1,1))
    abline(v=at1, col="black")
    abline(h=seq(-1,1,by=0.5), col="black")
    axis(2, at=c(-1,0,1), label=c(-1,0,1), cex.axis=2)
    axis(4, at=c(-1,0,1), label=c(-1,0,1), cex.axis=2)
    if (epo$gene.width.px >= 10) { type <- "b" } else { type <- "l" }
    lines( (seq(no.genes)-1)/ (no.genes-1),
        epo$gene.score, type=type, lwd=3, col="brown", xpd=NA, pch=20)

#########
    
    par(mar=c(0,1,0,4))
    image(matrix(c(0,0,0,0), nr=1, nc=1), zlim=epo$zlim,
          col="#00000000", axes=FALSE,
          xlab=NA, ylab=NA, xlim=c(0,1), ylim=c(0,1))  
    text(seq(0,1,by=0.5), .5/(ncol(epo$exp)+1), c(-1,0,1), pos=1, xpd=NA,
         cex=2)
    at3 <- (no.conds-label2+0.5)/no.conds
    for ( p in seq(0,1,by=0.25)) {
      segments(p, 0.5/(no.conds+1), p, (no.conds-0.5)/no.conds)
    }
    abline( h=at3, col="black")
    abline( h=(no.conds-0.5)/no.conds, col="black")
    axis(4, at=at3, label=label2, cex.axis=2)
    lines((epo$cond.score+1)/2, pch=20,
          (seq(ncol(epo$exp))-0.5)/ncol(epo$exp), type="b",
          lwd=3, col="brown", xpd=NA  )
  }
  
  invisible(list(coords=bbox, gene.width=epo$gene.width.px,
                 cond.height=epo$cond.height.px))
}  

condPlot <- function(modules, number, eset,
                      col="white", all=TRUE,
                      sep=NULL, sepcol="grey", val=TRUE, srt=90,
                      adj.above=c(0,0.5), adj.below=c(1,0.5),
                      plot.only=seq_len(ncol(eset)), ...) {

  isa2:::isa.status("Creating a condition plot", "in")

  eset <- eisa.get.nm(eset, modules)
  nm1 <- t(featExprs(eset))

  genes <- getFeatureMatrix(modules, mods=number)
  samp <- getSampleMatrix(modules, mods=number)
  thr <- sampleThreshold(modules)[number]
  
  ## Calculate all condition scores, might not be correct for
  ## oscillating modules
  scores <- as.vector(nm1 %*% genes)
  msc <- mean(scores)
  ssc <- sd(scores)
  thr1 <- msc + thr * ssc
  thr2 <- msc - thr * ssc

  n.scores <- ifelse(samp != 0, scores, 0)
  fact <- max(abs(n.scores))
  if (fact != 0) scores <- scores / fact
  msc <- msc / fact
  thr1 <- thr1 / fact
  thr2 <- thr2 / fact
  scores <- scores[plot.only]
  to.plot <- scores
  if (!all) {
    to.plot [ to.plot > thr2 & to.plot < thr1 ] <- 0
  }
  
  ylim <- range(to.plot)*1.4
  if (ylim[1] > 0) ylim[1] <- 0
  if (ylim[2] < 0) ylim[2] <- 0.2
  par(mar=c(1,3,1,2))
  barplot(to.plot, space=0, col=col, ylim=ylim, ...)

  abline(h=thr1, col="red")
  abline(h=thr2, col="darkgreen")
  abline(h=msc, col="grey", lty=2)

  if (!is.null(sep)) {

    abline(v=sep, lty=2, col=sepcol)
    text(sep, ylim[2], pos=2, names(sep), font=3, srt=90, cex=0.8, xpd=NA)
    points(to.plot, type="h")
    points(seq(along=to.plot)-1, to.plot, type="h")
  }
  
  if (val) {
    above <- which(scores>=0)
    below <- which(scores<0)
    font <- ifelse(scores>thr1 | scores<thr2, 2, 1)
    tt <- round(scores,2)*100
    if (length(above)>0) {
      text(seq(scores)[above]-0.5, scores[above]+0.05, tt[above],
           adj=adj.above, cex=0.7, col="red", srt=srt, font=font[above])
    }
    if (length(below)>0) {
      text(seq(scores)[below]-0.5, scores[below]-0.05, tt[below],
           adj=adj.below, cex=0.7, col="darkgreen", srt=srt, font=font[below])
    }
    par(xpd=TRUE)
    text(length(scores), thr1+0.1, round(thr1,2), pos=4, font=2)
    text(length(scores), thr2-0.1, round(thr2,2), pos=4, font=2)
    par(xpd=FALSE)
  }

  isa2:::isa.status("DONE", "out")
}

overlap <- function(modules, algorithm=c("mds", "fr", "drl"), edge.limit=0.5) {

  isa2:::isa.status("Creating an overlap plot", "in")
  
  algorithm <- match.arg(algorithm)

  genes <- getFeatureMatrix(modules)

  library(igraph0)

  if (algorithm=="mds") {
    library(MASS)
    B <- cor(genes)
    A <- abs(B)
    B [ A < edge.limit ] <- 0
    coords <- isoMDS(1-A)$points
    diag(B) <- diag(A) <- 0
  
    g <- graph.adjacency(B, weighted=TRUE, mode="undirected")
    V(g)$x <- coords[,1]
    V(g)$y <- coords[,2]
  } else if (algorithm=="fr") {
    B <- cor(genes)
    A <- abs(B)
    diag(B) <- diag(A) <- 0
    g2 <- graph.adjacency(A, weighted=TRUE, mode="undirected")  
    B [ A < edge.limit ] <- 0
    if (all(B==0)) {
      g <- graph.empty(n=nrow(B), dir=FALSE)
      E(g)$weight <- numeric()
    } else {
      g <- graph.adjacency(B, weighted=TRUE, mode="undirected")
    }

    l <- layout.fruchterman.reingold(g2, weights=E(g2)$weight*10)
    V(g)$x <- l[,1]
    V(g)$y <- l[,2]
  } else if (algorithm=="drl") {
    B <- cor(genes)
    A <- abs(B)
    diag(B) <- diag(A) <- 0
    g2 <- graph.adjacency(A, weighted=TRUE, mode="undirected")  
    B [ A < edge.limit ] <- 0
    if (all(B==0)) {
      g <- graph.empty(n=nrow(B), dir=FALSE)
      E(g)$weight <- numeric()
    } else {
      g <- graph.adjacency(B, weighted=TRUE, mode="undirected")
    }

    l <- layout.drl(g2, weights=E(g2)$weight*10)
    V(g)$x <- l[,1]
    V(g)$y <- l[,2]
  }    

  V(g)$label <- seq_len(ncol(genes))
  if (ecount(g) != 0) {
    E(g)$color <- ifelse( E(g)$weight > 0, "grey", "red" )
    E(g)$width <- abs(E(g)$weight) * 3
  }

  isa2:::isa.status("DONE", "out")
  
  g
}

overlapPlot <- function(graph, xsize=400, ysize=400,
                         vertex.size=20, vertex.size2=10, ...) {

  l <- cbind(V(graph)$x, V(graph)$y)
  l <- layout.norm(l, -1, 1, -1, 1)
  par(mar=c(0,0,0,0))
  plot(graph, layout=l, rescale=FALSE, asp=FALSE, vertex.shape="rectangle", vertex.label.cex=1,
       vertex.size=vertex.size, vertex.size2=vertex.size2, xlim=c(-1.2, 1.2), ylim=c(-1.2, 1.2), ...)

  coords <- cnvrt.coords(l[,1]-20/200, l[,2]+10/200)$tdev
  coords <- cbind(coords$x, coords$y)
  coords[,1] <- coords[,1] * xsize
  coords[,2] <- ysize - coords[,2] * ysize + 1
  mode(coords) <- "integer"
  invisible(coords)
}

mnplot <- function(x, expset, group, ...) {
  if (length(levels(factor(group))) != 2) 
    stop("only works for factors with two levels")
  
  if (is(expset, "ExpressionSet")) {
    cont1 <- x %in% featureNames(expset)
    if (any(!cont1)) {
      warning("Some features were dropped.")
      x <- x[cont1]
    }
    dataM1 <- exprs(expset)[x,,drop=FALSE]
  } else {
    cont1 <- x %in% rownames(expset)
    if (!any(cont1)) {
      warning("Some features were dropped.")
      x <- x[cont1]
    }
    dataM1 <- expset[x,,drop=FALSE]
  }
  tts = apply(dataM1, 1, function(x) sapply(split(x, group), mean))
  rn = row.names(tts)
  plot(tts[1, ], tts[2, ], xlab = rn[1], ylab = rn[2], ...)
  abline(a = 0, b = 1)
  invisible(tts)
}

select.eset <- function(eset, modules, norm=c("raw", "feature", "sample")) {
  norm <- match.arg(norm)
  if (norm=="raw") {
    eset <- exprs(eset)
  } else if (norm=="feature") {
    eset <- eisa.get.nm(eset, modules)
    eset <- featExprs(eset)
  } else if (norm=="sample") {
    eset <- eisa.get.nm(eset, modules)
    eset <- sampExprs(eset)
  }
  eset
}

ISAmnplot <- function(modules, number, eset,
                      norm=c("raw", "feature", "sample"),
                      group, ...) {

  if (length(levels(factor(group))) != 2) 
    stop("only works for factors with two levels")
  
  x <- getFeatureMatrix(modules, mods=number)
  xx <- rownames(x)[x > 0]
  yy <- rownames(x)[x < 0]
  eset <- select.eset(eset, modules, norm)
  mnplot(c(xx, yy), expset=eset, group=group,
         col=c(rep("red", length(xx)), rep("green", length(yy))), ...)
}

ISA2heatmap <- function(modules, module, eset,
                        norm=c("raw", "feature", "sample"),
                        scale=c("none", "row", "column"), ...) {

  norm <- match.arg(norm)
  scale <- match.arg(scale)
  eset <- select.eset(eset, modules, norm)
  x <- getFeatureNames(modules, module)[[1]]
  y <- getSampleNames (modules, module)[[1]]
  dataM <- eset[x,y]
  if (is(eset, "ExpressionSet")) {
    dataM <- exprs(dataM)
  }
  heatmap(dataM, scale=scale, ...)
}

profilePlot <- function(modules, module, eset,
                        plot=c("samples", "features", "both"),
                        norm="default",
                        background=TRUE, col=gray(0.7), col.mod=1,
                        type="l", type.mod=type,
                        mean=TRUE, meancol="green", meancol.mod="red",
                        xlabs=c("Features","Samples"),
                        ylab="Expression", ...) {

  plot <- match.arg(plot)

  # How to normalize the expression matrix for the plots
  if (any(!norm %in% c("default", "feature", "sample", "raw"))) {
    stop("`norm' must be one of `default', `feature', `sample' or `raw'")
  }
  norm <- rep(norm, length=2)
  if (norm[1]=="default") {
    if (plot=="samples" || plot=="both") {
      norm[1] <- "feature"
    } else {
      norm[1] <- "sample"
    }
  }
  if (norm[2]=="default" && plot=="both") {
    norm[2] <- "sample"
  }
   
  data <- select.eset(eset, modules, norm[1])
  if (norm[2] != norm[1] && plot=="both") {
    data2 <- select.eset(eset, modules, norm[2])
  } else {
    data2 <- data
  }
  
  if (is(data, "ExpressionSet") || !is.null(rownames(data))) {
    data <- data[featureNames(modules),]
  }
  if (is(data2, "ExpressionSet") || !is.null(rownames(data2))) {
    data2 <- data2[featureNames(modules),]
  }

  if (!all(dim(data) == dim(modules))) {
    stop("data sizes do not match")
  }
  
  feats <- getFeatures(modules, module)[[1]]
  samps <- getSamples(modules, module)[[1]]

  pp <- function(data, xx, yy, xlab) {

    if (length(xx)==0) {
      stop("No features to plot")
    }
    
    nyy <- if (length(yy)!=0) { -yy } else { seq(ncol(data)) }
    data <- data[xx,]
    
    xlim <- c(1, length(xx))
    ylim <- range(data)
    
    par(mar=c(2,4,1,1)+0.1)
    plot(NA, type="n", xlim=xlim, ylim=ylim, xlab=NA,
         ylab=ylab, axes=FALSE, ...)
    title(xlab=xlab, mgp=c(0,0,0))
    axis(2)
    if (background) {
      for (i in seq_len(ncol(data))[nyy]) {
        lines(data[,i], col=col, type=type, ...)
      }
    }
    for (i in seq_len(ncol(data))[yy]) {
      lines(data[,i], col=col.mod, type=type.mod, ...)
    }
    if (mean) {
      if (background) { lines(rowMeans(data[,yy]), col=meancol.mod, type=type.mod) }
      lines(rowMeans(data[,nyy]), col=meancol, type=type)
    }
  }

  if (plot=="samples") {
    pp(data, xx=feats, yy=samps, xlab=xlabs[1])
  } else if (plot=="features") {
    pp(t(data), xx=samps, yy=feats, xlab=xlabs[2])
  } else {
    par(mfrow=c(2,1))
    pp(data, xx=feats, yy=samps, xlab=xlabs[1])
    pp(t(data2), xx=samps, yy=feats, xlab=xlabs[2])
  }

  invisible(NULL)
}
