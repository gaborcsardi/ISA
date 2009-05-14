\name{isa.in.silico}
\alias{isa.in.silico}
\title{Generate in-silico input data for biclustering algorithms}
\description{This function generates a test data set for ISA, containing
  modules of prescribed number, size, signal level, internal noise and
  background noise. 
}
\usage{
isa.in.silico (num.rows = 300, num.cols = 50, num.fact = 3,
     mod.row.size = round(0.5 * num.rows/num.fact), 
     mod.col.size = round(0.5 * num.cols/num.fact), noise = 0.1,                 
     mod.signal = rep(1, num.fact), mod.noise = rep(0, num.fact),                
     overlap.row = 0, overlap.col = overlap.row)
}
\arguments{
  \item{num.rows}{The number of rows in the data matrix.}
  \item{num.cols}{The number of columns in the data matrix.}
  \item{num.fact}{The number of modules to put into the data.}
  \item{mod.row.size}{The size of the modules, the number of rows per
    module. It can be a scalar or a vector and it is recycled.}
  \item{mod.col.size}{The size of the modules, the number of columns per
    module. It can be a scalar or a vector and it is recycled.}
  \item{noise}{The level of the background noise to be added to the data
    matrix. It gives the standard deviation of the normal distribution
    from which the noise is generated.}
  \item{mod.signal}{The signal level of the modules.}
  \item{mod.noise}{The noise levels of the different modules. Normally
    distributed noise with standard deviation \code{mod.noise} is added
    to the data. This is in addition to the background noise.}
  \item{overlap.row}{The overlap of the modules, for the rows.
    Zero means no overlap, one means one overlapping row, etc.} 
  \item{overlap.col}{The overlap of the modules, for the columns.
    Zero means no overlap, one means one overlapping column, etc.} 
}
\details{
  \code{isa.in.silico} creates an artificial data set to test the ISA or
  any other biclustering algorithm. It creates a data matrix with a
  checkerboard matrix. In other words, potentially overlapping blocks
  are planted into a noisy background matrix.

  These blocks may have different signal and noise levels and they might
  also overlap. See the parameters above.
}
\value{
  A list with three matrices. The first matrix is the in silico data,
  the second contains the rows of the correct modules, the third the
  columns.
}
\author{ Gabor Csardi \email{Gabor.Csardi@unil.ch} }
\references{
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.
  
  Ihmels J, Friedlander G, Bergmann S, Sarig O, Ziv Y, Barkai N:
  Revealing modular organization in the yeast transcriptional network
  \emph{Nat Genet.} 2002 Aug;31(4):370-7. Epub 2002 Jul 22

  Ihmels J, Bergmann S, Barkai N:
  Defining transcription modules using large-scale gene expression data
  \emph{Bioinformatics} 2004 Sep 1;20(13):1993-2003. Epub 2004 Mar 25.
}
\seealso{ \link{isa2-package} for a short introduction on the Iterative
  Signature Algorithm. See \code{\link{isa}} for an easy way of running
  ISA.
}
\examples{
## Define a function for plotting if we are interactive
if (interactive()) { layout( rbind(1:2,3:4) ) }
myimage <- function(mat) {
  if (interactive()) par(mar=c(1,2,2,1)); image(mat[[1]])
}

## Create a simple checkerboard without overlap and noise
silico1 <- isa.in.silico(100, 100, 10, mod.row.size=10, mod.col.size=10,
                         noise=0)
myimage(silico1)

## The same, but with some overlap and noise
silico2 <- isa.in.silico(100, 100, 10, mod.row.size=10, mod.col.size=10,
                         noise=0.1, overlap.row=3)
myimage(silico2)

## Modules with different noise levels
silico3 <- isa.in.silico(100, 100, 5, mod.row.size=10, mod.col.size=10,
                         noise=0.01, mod.noise=seq(0.1,by=0.1,length=5))
myimage(silico3)

## Modules with different signal levels
silico4 <- isa.in.silico(100, 100, 5, mod.row.size=10, mod.col.size=10,
                         noise=0.01, mod.signal=seq(1,5,length=5))
myimage(silico4)
}
\keyword{cluster}
