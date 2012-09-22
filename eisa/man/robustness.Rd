\name{ISAFilterRobust}
\alias{ISAFilterRobust}
\alias{ISARobustness}
\title{Robustness of ISA biclusters}
\description{Robustness of ISA biclusters. The more robust biclusters
  are more significant, in the sense that they are less likely to be
  found in random data.}
\usage{
ISARobustness(data, isaresult)
ISAFilterRobust(data, isaresult, \dots)
}
\arguments{
  \item{data}{An \code{ExpressionSet} or \code{ISAExpressionSet}
    object. If an \code{ExpressionSet} object is
    supplied, then it is normalised by calling
    \code{\link{ISANormalize}} on it.}
  \item{isaresult}{An \code{ISAModules} object, a set of modules.}
  \item{\dots}{Additional arguments, they are passed to the
    \code{isa.filter.robust} function in the \code{isa2} package.}
}
\details{
  \code{ISARobustness} calculates robustness scores for ISA modules. The
  higher the score, the more robust the module.

  \code{ISAFilterRobust} filters a set of ISA modules, by running ISA
  on the randomized expression data and then eliminating all modules
  that have a robustness score that is lower than at least one
  robustness score found in the randomized data.

  The same feature and sample thresholds are used to calculate the
  randomized robustness scores. In other words the limit for the
  filtering depends on the feature and sample thresholds.

  You can find more details in the manual of the
  \code{\link[isa2]{robustness}} function in the \code{isa2} package.
}
\value{
  \code{ISARobustness} returns a numeric vector, the robustness scores
  of the biclusters.

  \code{ISAFilterRobust} returns the filtered \code{ISAModules}
  instance.
}
\author{
  Gabor Csardi \email{csardi.gabor@gmail.com} 
}
\references{
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.
}
\seealso{
  The \code{\link[isa2]{robustness}} function in the \code{isa2}
  package.
}
\examples{
data(ALLModules)
library(ALL)
data(ALL)
rob <- ISARobustness(ALL, ALLModules)
summary(rob)
}
\keyword{cluster}
