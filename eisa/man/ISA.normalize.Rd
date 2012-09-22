\name{ISANormalize}
\alias{ISANormalize}
\title{Normalize expression data for the Iterative Signature Algorithm}
\description{
  ISA works best if the input data is centered and
  scaled. \code{ISANormalize} performs this transformation.
}
\usage{
ISANormalize (data, prenormalize = FALSE)
}
\arguments{
  \item{data}{An \code{ExpressionSet} object.}
  \item{prenormalize}{  If this argument is set to \code{TRUE}, then 
    feature-wise scaling is calculated on the sample-wise scaled matrix
    and not on the input matrix directly.}
}
\details{
  It was observed that the ISA works better if the input matrix is
  scaled and its rows have mean zero and standard deviation one.

  An ISA step consists of two sub-steps, and this implies two different
  normalizations, in the first the rows (=features), in the second the
  columns (=samples) of the input matrix will be scaled and centered.
}
\value{
  An \code{\link{ISAExpressionSet}} object.
}
\author{ Gabor Csardi \email{csardi.gabor@gmail.com} }
\references{
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.
}
\seealso{The \code{\link{ISA}} function for an easier ISA workflow.}
\examples{
library(ALL)
data(ALL)

# Do the normalization
ALL.normed <- ISANormalize(ALL)
class(ALL.normed)
dim(exprs(ALL.normed))
dim(featExprs(ALL.normed))
dim(sampExprs(ALL.normed))

# Check that we indeed have Z-scores
all(abs(apply(featExprs(ALL.normed), 2, mean) ) < 1e-12)
all(abs(1-apply(featExprs(ALL.normed), 2, sd)) < 1e-12)

all(abs(apply(sampExprs(ALL.normed), 1, mean) ) < 1e-12)
all(abs(1-apply(sampExprs(ALL.normed), 1, sd)) < 1e-12)
}
\keyword{cluster}
