
isa.in.silico <- function(num.rows=300, num.cols=50, num.fact=3, fact.per.row=1,
                          fact.per.col=fact.per.row,
                          mod.row.size=round(.5*num.rows/num.fact),
                          mod.col.size=round(.5*num.cols/num.fact),
                          noise=0.1,
                          mod.signal=rep(1, num.fact),
                          mod.noise=rep(0, num.fact), OL=0) {

  if (max(mod.row.size) > num.rows || max(mod.col.size) > num.cols) {
    stop("Inconsistent data configuration")
  }

  if (length(mod.noise) != num.fact) {
    stop("Invalid `mod.noise' length")
  }
  
  mod.row.size <- sort(rep(mod.row.size, length=num.fact), decreasing=TRUE)
  mod.col.size <- sort(rep(mod.col.size, length=num.fact), decreasing=TRUE)

  mod.signal <- rep(mod.signal, length=num.fact)
  
  rowMod <- matrix(0, nr=num.rows, nc=num.fact)
  colMod <- matrix(0, nr=num.cols, nc=num.fact)

  rowP <- colP <- 0
  data <- matrix(0, nr=num.cols, nc=num.rows)

  for (i in 1:num.fact) {
    rowMod[(max(1,1+rowP-round(OL*mod.row.size[i]))):
            (rowP+round((1-OL)*mod.row.size[i])), i] <- sqrt(mod.signal[i])
    colMod[(max(1,1+colP-round(OL*mod.col.size[i]))):
            (colP+round((1-OL)*mod.col.size[i])), i] <- sqrt(mod.signal[i])
    rowP <- rowP+round((1-OL)*mod.row.size[i])
    colP <- colP+round((1-OL)*mod.col.size[i])

    sel.a <- which(rowMod[,i] != 0)
    sel.b <- ceiling(num.cols * matrix(runif(length(sel.a)*fact.per.row),
                                        nr=length(sel.a), nc=fact.per.row))
    for (j in 1:length(sel.a)) {
      for (k in seq(length=fact.per.row)) {
        data[ sel.b[j,k], sel.a[j] ] <- mod.signal[i]
      }
    }
    sel.a <- which(colMod[,i] != 0)
    sel.b <- ceiling(num.rows * matrix(runif(length(sel.a)*fact.per.col),
                                        nr=length(sel.a), nc=fact.per.col))
    for (j in 1:length(sel.a)) {
      for (k in seq(length=fact.per.col)) {
        data[ sel.a[j], sel.b[j,k] ] <- mod.signal[i]
      }
    }
  }
  
  data[] <- pmax(data, colMod %*% t(rowMod))
  data[] <- data + rnorm(length(data), mean=0, sd=noise)
  rowMod[] <- ifelse(rowMod != 0, 1, 0)
  colMod[] <- ifelse(colMod != 0, 1, 0)
  
  rowP <- colP <- 0
  for (i in 1:num.fact) {
    g.from <- (max(1,1+rowP-round(OL*mod.row.size[i])))
    g.to <- (rowP+round((1-OL)*mod.row.size[i]))
    c.from <- (max(1,1+colP-round(OL*mod.col.size[i])))
    c.to <- (colP+round((1-OL)*mod.col.size[i]))
    data[c.from:c.to, g.from:g.to] <- data[c.from:c.to, g.from:g.to] +
      rnorm( (g.to-g.from+1) * (c.to-c.from+1), mean=0, sd=mod.noise[i] )
    rowP <- rowP+round((1-OL)*mod.row.size[i])
    colP <- colP+round((1-OL)*mod.col.size[i])
  }
  
  list(data=data, rowMod=rowMod, colMod=colMod)
}
