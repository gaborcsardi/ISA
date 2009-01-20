
ROC <- function(prediction, truth) {

  if (length(prediction) != length(truth)) {
    stop("prediction and truth vectors/matrices have different size")
  }
  
  if ( is.matrix(prediction) && is.matrix(truth) &&
      any (dim(prediction) != dim(truth)) ) {
    warning("prediction and truth matrices have differnt dimensions")
  }
  
  ds <- length(prediction)

  denom.Sn <- sum(truth==1)
  denom.ER <- ds - denom.Sn
  
  si <- order(prediction)
  sp <- prediction[si]
  str <- truth[si]

  p <- (str == 1)
  TP <- seq(denom.Sn, by=-1, 1)
  tm <- seq(ds, by=-1, 1)
  tm <- tm[p]
  FP <- tm - TP

  ER <- FP/denom.ER
  ER <- c(1,ER)
  Sn <- TP/denom.Sn
  Sn <- c(1,Sn)
  roc <- cbind(ER, Sn)

  attributes(roc)$integral <- ROC.integral(roc, low=0, up=1)
  roc
}

ROC.integral <- function(roc, low=0, up=1) {

  Sn <- roc[,"Sn"]
  ER <- roc[,"ER"]

  valid <- which(ER >= low & ER <= up)
  v1 <- valid[1]
  v2 <- valid[length(valid)]

  ER <- ER[v1:v2]
  Sn <- Sn[v1:v2]
  
  Sn.a <- Sn[-length(Sn)]
  Sn.b <- Sn[-1]
  midP <- (Sn.a+Sn.b)/2
  ER.a <- ER[-length(ER)]
  ER.b <- ER[-1]
  intP <- ER.a - ER.b
  mes <- sum(intP * midP)

  mes
}  
