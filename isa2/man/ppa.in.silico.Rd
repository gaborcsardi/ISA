\name{ppa.in.silico}
\alias{ppa.in.silico}
\title{Generate in-silico input data for testing the PPA algorithm}
\description{This function generates an artificial test data set for the
  PPA algorithm: two matrices, with common column dimension, containing
  co-modules of prescribed number, size, signal level, noise level and
  background noise.
}
\usage{
ppa.in.silico (num.rows1 = 300, num.rows2 = 200, num.cols = 50,
    num.fact = 3, mod.row1.size = round(0.5 * num.rows1/num.fact),
    mod.row2.size = round(0.5 * num.rows2/num.fact),
    mod.col.size = round(0.5 * num.cols/num.fact), 
    noise = 0.1, mod.signal = rep(1, num.fact),
    mod.noise = rep(0, num.fact), overlap.row1 = 0,
    overlap.row2 = overlap.row1, overlap.col = overlap.row1) 
}
\arguments{
  \item{num.rows1}{The number of rows in the first data matrix.}
  \item{num.rows2}{The number of rows in the second data matrix.}
  \item{num.cols}{The number of columns in both matrices.}
  \item{num.fact}{The number of co-modules to put in the data.}
  \item{mod.row1.size}{The size of the co-modules, the number of rows
    from the first data matrix. It can be a scalar or a vector, and it
    is recycled.}
  \item{mod.row2.size}{The size of the co-modules, the number of rows
    from the second data matrix. It can be a scalar or a vector, and it
    is recycled.}
  \item{mod.col.size}{The size of the co-modules, the number of columns
    (from both matrices). It can be a scalar or a vector, and it
    is recycled.}
  \item{noise}{The level of the background noise to be added to the data
    matrices. It gives the standard deviation of the normal distribution
    from which the noise is generated.}
  \item{mod.signal}{The signal level of the co-modules.} 
  \item{mod.noise}{The noise levels of the different co-modules. Normally
    distributed noise with standard deviation \code{mod.noise} is added
    to the data. This is in addition to the background noise.}
  \item{overlap.row1}{The overlap of the modules, for the rows of the
    first matrix. Zero means no overlap, one means one overlapping row,
    etc.}
  \item{overlap.row2}{The same as \code{overlap.row1}, but for the rows
    of the second matrix.}
  \item{overlap.col}{The same as \code{overlap.row1}, but for the
    columns of both matrices.}
}
\details{
  \code{ppa.in.silico} creates an artificial data set to test the PPA
  (or potentially other) algorithm. It creates two data matrices with an
  overlapping dimension and a checkerboard-like structure. The fields of
  the checkerboard are the co-modules, and they may have different
  signal and noise levels, and they may also overlap.  
}
\value{
  A list with five matrices. The first two are the two data matrices,
  they have the same number of columns. The last three matrices contain
  the correct modules, for the rows of the first matrix, the rows of the
  second matrix, and finally for the common column dimension.
}
\author{ Gabor Csardi \email{Gabor.Csardi@unil.ch} }
\references{
  Kutalik Z, Bergmann S, Beckmann, J: A modular approach for integrative
  analysis of large-scale gene-expression and drug-response data
  \emph{Nat Biotechnol} 2008 May; 26(5) 531-9.
}
\seealso{
  See \code{\link{ppa}} for an easy way of running the PPA
}
\examples{
## Define a function for plotting if we are interactive
if (interactive()) { layout(cbind(1:2,3:4,5:6,7:8)) }
myimage <- function(mat) {
  if (interactive()) {
    par(mar=c(3,2,1,1)); image(mat[[1]])
    par(mar=c(3,2,1,1)); image(mat[[2]])
  }
}

## Co-modules without overlap and noise
silico1 <- ppa.in.silico(100, 100, 100, 10, mod.row1.size=10,
                         mod.row2.size=10, mod.col.size=10, noise=0)
myimage(silico1)

## The same, but with overlap and noise
silico2 <- ppa.in.silico(100, 100, 100, 10, mod.row1.size=10,
                         mod.row2.size=10, mod.col.size=10, noise=0.1,
                         overlap.row1=3)
myimage(silico2)

## Co-modules with different noise levels
silico3 <- ppa.in.silico(100, 100, 100, 5, mod.row1.size=10,
                         mod.row2.size=10, mod.col.size=10, noise=0.01,
                         mod.noise=seq(0.1,by=0.1,length=5))
myimage(silico3)

## Co-modules withe different signal levels
silico4 <- ppa.in.silico(100, 100, 100, 5, mod.row1.size=10,
                         mod.row2.size=10, mod.col.size=10, noise=0.01,
                         mod.signal=seq(1,5,length=5))
myimage(silico4)
}
\keyword{cluster}
