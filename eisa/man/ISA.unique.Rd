\name{ISA.unique}
\alias{ISA.unique}
\title{Remove duplicated ISA modules}
\description{
  From a potentially non-unique set of ISA modules remove all modules
  that are similar to others.
}
\usage{
ISA.unique(data, isaresult, \dots)
}
\arguments{
  \item{data}{An \code{ExpressionSet} or \code{ISAExpressionSet}
    object. If \code{ExpressionSet} object is supplied, then it is
    normalised by calling \code{\link{ISA.normalize}} on it.}
  \item{isaresult}{An \code{ISAModules} object to be filtered.}
  \item{\dots}{Additional arguments, these are passed to the
    \code{\link[isa2]{isa.unique}} function in the \code{isa2}
    package. See also details below.}
}
\details{
  The ISA algorithm might very well find the same modules from many
  different input seeds, so the output of the \code{\link{ISA.iterate}}
  function is usually not unique: many modules are very similar to each
  other.

  \code{ISA.unique} eliminate the duplicates and potentially also the
  non-convergent modules.

  The work is performed by calling the \code{\link[isa2]{isa.iterate}}
  function in the \code{isa2} package. The following additional arguments
  can be specified to be passed to this function:
  \describe{
    \item{method}{Character scalar giving the method to be used to
      determine if two biclusters are similar. Right now only
      \sQuote{\code{cor}} is implemented, this keeps both biclusters if
      their Pearson correlation is less than \code{cor.limit}, both for
      their row and column scores. See also the \code{neg.cor} argument.}
    \item{ignore.div}{Logical scalar, if \code{TRUE}, then the divergent
      biclusters will be removed.}
    \item{cor.limit}{Numeric scalar, giving the correlation limit for the
      \sQuote{\code{cor}} method.}
    \item{neg.cor}{Logical scalar, if \code{TRUE}, then the
      \sQuote{\code{cor}} method considers the absolute value of the
      correlation.}
    \item{drop.zero}{Logical scalar, whether to drop biclusters that have
      all zero scores.}
  }
}
\value{Another \code{ISAModules} object, with unique modules.}
\author{ Gabor Csardi \email{Gabor.Csardi@unil.ch} }
\references{
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar
  11.
}
\seealso{The \code{\link{ISA}} function for an easier ISA workflow.}
\examples{}
\keyword{cluster}
