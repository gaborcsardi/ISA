\name{isa.normalize}
\alias{isa.normalize}
\alias{isa.normalize,matrix-method}
\title{Normalize input data for use with ISA}
\description{
  Normalize a matrix and create a form that can be effectively used for
  ISA runs.
}
\usage{
\S4method{isa.normalize}{matrix}(data, \dots)
}
\arguments{
  \item{data}{A numeric matrix, the input data. It might contains
    \code{NA} and/or \code{NaN} values.}
  \item{\dots}{Additional arguments, see details below.}
}
\details{
  This function can be called as
  \preformatted{
    isa.normalize(data, prenormalize = FALSE)    
  }
  where the arguments are:
  \describe{
    \item{data}{A numeric matrix, the input data. It might contains
      \code{NA} and/or \code{NaN} values.}
    \item{prenormalize}{Logical scalar, see details below.}
  }
  
  It was observed that the ISA works better if the input matrix is
  scaled and its rows have mean zero and standard deviation one.

  An ISA step consists of two sub-steps, and this implies two different
  normalizations, in the first the rows, in the second the columns of
  the input matrix will be scaled.

  If the \code{prenormalize} argument is set to \code{TRUE}, then
  row-wise scaling is calculated on the column-wise scaled matrix and
  not on the input matrix directly.
}
\value{
  A list of two normalized matrices, the first one is transposed.
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
## In-silico data
set.seed(1)
insili <- isa.in.silico()
nm <- isa.normalize(insili[[1]])

## Check it
max(abs(rowSums(nm[[1]])))
max(abs(rowSums(nm[[2]])))

max(abs(apply(t(nm[[1]]), 2, sd) - 1))
max(abs(apply(t(nm[[2]]), 2, sd) - 1))

## Plot them
if (interactive()) {
  layout(rbind(1:2,3:4))
  image(insili[[1]], main="Original data")
  image(t(nm[[1]]), main="Row normalized")
  image(nm[[2]], main="Column normalized")
}

}
\keyword{cluster}
