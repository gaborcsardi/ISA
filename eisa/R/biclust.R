
ISA.biclust <- function(modules) {

  if (!require(biclust)) {
    stop("The `biclust' package is required for this")
  }

  new("Biclust", Parameters=list(seeddata=modules@seeddata,
                   rundata=modules@rundata),
      RowxNumber=modules@genes != 0,
      NumberxCol=t(modules@conditions != 0), Number=ncol(modules@genes))
}
