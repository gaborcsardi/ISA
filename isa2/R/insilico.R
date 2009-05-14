
isa.in.silico <- function(num.rows=300, num.cols=50, num.fact=3,
                          mod.row.size=round(.5*num.rows/num.fact),
                          mod.col.size=round(.5*num.cols/num.fact),
                          noise=0.1,
                          mod.signal=rep(1, num.fact),
                          mod.noise=rep(0, num.fact),
                          overlap.row=0, overlap.col=overlap.row) {

  isa.status("Creating in-silico data set", "in")
  
  if (max(mod.row.size) > num.rows || max(mod.col.size) > num.cols) {
    stop("Inconsistent data configuration")
  }

  if (length(mod.noise) != num.fact) {
    stop("Invalid `mod.noise' length")
  }
  
  mod.row.size <- rep(mod.row.size, length=num.fact)
  mod.col.size <- rep(mod.col.size, length=num.fact)

  ol.row <- overlap.row * seq(0,num.fact-1)
  ol.col <- overlap.col * seq(0,num.fact-1)
  crow.from <- cumsum(c(1,mod.row.size))[-(num.fact+1)] - ol.row
  crow.to   <- cumsum(mod.row.size) - ol.row
  ccol.from <- cumsum(c(1,mod.col.size))[-(num.fact+1)] - ol.col
  ccol.to   <- cumsum(mod.col.size) - ol.col

  mod.signal <- rep(mod.signal, length=num.fact)
  
  rowMod <- matrix(0, nr=num.rows, nc=num.fact)
  colMod <- matrix(0, nr=num.cols, nc=num.fact)

  data <- matrix(0, nr=num.cols, nc=num.rows)

  for (i in seq_len(num.fact)) {
    rowMod[crow.from[i]:crow.to[i],i] <- sqrt(mod.signal[i])
    colMod[ccol.from[i]:ccol.to[i],i] <- sqrt(mod.signal[i])
  }
  
  data[] <- colMod %*% t(rowMod)
  data[] <- data + rnorm(length(data), mean=0, sd=noise)
  rowMod[] <- ifelse(rowMod != 0, 1, 0)
  colMod[] <- ifelse(colMod != 0, 1, 0)

  ## Add module specific noise
  for (i in which(mod.noise != 0)) {
    idx2 <- crow.from[i]:crow.to[i]
    idx1 <- ccol.from[i]:ccol.to[i]
    rnd <- rnorm(length(idx1)*length(idx2), sd=mod.noise)
    data[idx1,idx2] <- data[idx1,idx2] + rnd
  }
  
  res <- list(data=t(data), rowMod=rowMod, colMod=colMod)

  isa.status("DONE", "out")

  res
}
