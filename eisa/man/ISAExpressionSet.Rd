\name{ISAExpressionSet-class}
\docType{class}
\alias{featExprs}
\alias{sampExprs}
\alias{ISAExpressionSet}
\alias{ISAExpressionSet-class}
\alias{featExprs,ISAExpressionSet-method}
\alias{sampExprs,ISAExpressionSet-method}
\alias{hasNA}
\alias{hasNA<-}
\alias{hasNA,ISAExpressionSet-method}
\alias{hasNA<-,ISAExpressionSet-method}
\alias{prenormalized}
\alias{prenormalized,ISAExpressionSet-method}
\alias{prenormalized<-}
\alias{prenormalized<-,ISAExpressionSet-method}
\title{Expression Set, normalized for using with ISA}
\description{
  An \code{ExpressionSet} object (\code{Biobase} package) that contains
  expression values normalized for use with the Iterative Signature
  Algorithm.
}
\usage{
\S4method{featExprs}{ISAExpressionSet}(object)
\S4method{sampExprs}{ISAExpressionSet}(object)

\S4method{hasNA}{ISAExpressionSet}(object)
\S4method{hasNA}{ISAExpressionSet}(object) <- value

\S4method{prenormalized}{ISAExpressionSet}(object)
\S4method{prenormalized}{ISAExpressionSet}(object) <- value
}
\arguments{
  \item{object}{An \code{ISAExpressionSet} object.}
  \item{value}{A logical scalar, new value of the \code{hasNA} or
    \code{prenormalized} attribute.}
}
\details{
  An \code{ISAExpressionSet} contains three expression matrices.

  In most cases, when then \code{ISAExpressionSet} was produced by the
  \code{\link{ISANormalize}} function, these are: the original, raw
  data, the feature-wise scaled and centered data and the sample-wise
  scaled and centered data.

  Two additional methods were defined to access the extra matrices:
  \code{featExprs} returns the feature-wise standardized data,
  \code{sampExprs} the sample-wise standardized one.

  The \code{hasNA} function returns \code{TRUE} if \code{NA} or
  \code{NaN} values appear in at least one of the expression matrices.

  The \code{prenormalized} function returns \code{TRUE} if the data was
  prenormalized, see \code{\link{ISANormalize}} for details.
}
\value{
  \code{featExprs} and \code{sampExprs} both return a matrix.

  \code{hasNA} and \code{prenormalized} return a logical vector of
  length one.
}
\author{ Gabor Csardi \email{csardi.gabor@gmail.com} }
\references{
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.
}
\seealso{
  \code{\link{ISANormalize}}, \code{ExpressionSet} in the \code{Biobase}
  package.
}
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
\keyword{classes}
