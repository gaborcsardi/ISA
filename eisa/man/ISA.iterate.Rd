\name{ISAIterate}
\alias{ISAIterate}
\title{Perform the Iterative Signature Algorithm}
\description{
  \code{ISAIterate} performs the ISA on an \code{ExpressionSet}
  object, from the given input seeds.
}
\usage{
ISAIterate(data, feature.seeds, sample.seeds, thr.feat,
    thr.samp = thr.feat, \dots)
}
\arguments{
  \item{data}{An \code{ExpressionSet} or \code{ISAExpressionSet}
    object. If an \code{ExpressionSet} object is supplied, then it is
    normalised by calling \code{\link{ISANormalize}} on it.}
  \item{feature.seeds}{A matrix of feature seeds. The number of rows
    should match the number of features in the \code{ExpressionSet},
    each column is a seed. Either this, or the \code{sample.seeds}
    argument must be given.}
  \item{sample.seeds}{A matrix of sample seeds. The number of rows
    should match the number of samples in the \code{ExpressionSet}, each
    column in a seed. Either this, or the \code{feature.seeds} argument
    must be given.}
  \item{thr.feat}{Numeric scalar or vector giving the threshold parameter
    for the features. Higher values indicate a more stringent threshold and
    the result biclusters will contain less features on average. The
    threshold is measured by the number of standard deviations from the
    mean, over the values of the feature vector. If it is a vector, then it
    must contain an entry for each seed.}
  \item{thr.samp}{Numeric scalar or vector giving the threshold parameter
    for the columns. The analogue of \code{thr.feat}.}
  \item{\dots}{Additional arguments, these are passed to the
    \code{\link[isa2]{isa.iterate}} function in the \code{isa2}
    package. See also details below.}
}
\details{
  Performs the ISA from the given seeds. It is allowed to specify both
  type of seeds, then a half-iteration is performed on the
  \code{sample.seeds} and they are appended to the
  \code{feature.seeds}.

  The \code{\link[isa2]{isa.iterate}} function of the \code{isa2}
  package is called to do all the work, this has the following extra
  parameters: \code{direction}, \code{convergence}, \code{cor.limit},
  \code{eps}, \code{corx}, \code{oscillation}, \code{maxiter}. Please see the
  \code{\link[isa2]{isa.iterate}} manual for details about them.
}
\value{An \code{\link{ISAModules}} object.}
\author{ Gabor Csardi \email{csardi.gabor@gmail.com} }
\references{
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.
}
\seealso{
  The \code{\link{ISA}} function for an easier interface with
  parameters.
}
\examples{
library(ALL)
data(ALL)

# Only use a small sample, to make this example finish faster
ALL.normed <- ISANormalize(ALL)[sample(1:nrow(ALL), 1000),]

# Generate seeds and do ISA
seeds <- generate.seeds(nrow(ALL.normed), count=100)
modules <- ISAIterate(ALL.normed, seeds, thr.feat=3, thr.samp=2)
modules
}
\keyword{cluster}
