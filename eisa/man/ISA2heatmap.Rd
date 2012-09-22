\name{ISA2heatmap}
\alias{ISA2heatmap}
\title{Heatmap of a transcription module}
\description{
  Create a heatmap plot for an ISA module.
}
\usage{
ISA2heatmap (modules, module, eset, norm = c("raw", "feature", "sample"), 
    scale = c("none", "row", "column"), \dots) 
}
\arguments{
  \item{modules}{An \code{\link{ISAModules}} object.}
  \item{module}{Numeric scalar, the number of the module to plot.}
  \item{eset}{An \code{ExpressionSet} or \code{ISAExpressionSet}
    object. If an \code{ExpressionSet} object is
    supplied (and the \code{norm} argument is not set to \sQuote{raw}),
    then it is normalised by calling \code{\link{ISANormalize}} on it.
    A subset of \code{eset} is selected that corresponds to
    the features included in \code{modules}.
  }
  \item{norm}{Character constant, specifies whether and how to normalize
    the expression values to plot. \sQuote{raw} plots the raw expression
    values, \sQuote{feature} the expression values scaled and centered
    for each feature (=gene) separately and if \sQuote{sample} is
    specified then the expression values are centered and scaled
    separately for each sample.}
  \item{scale}{Character scalar, passed to the
    \code{\link[stats]{heatmap}} function.}
  \item{\dots}{Additional arguments, they are passed to the
    \code{\link[stats]{heatmap}} function.}
}
%\details{}
\value{The same as \code{\link[stats]{heatmap}}.}
\author{ Gabor Csardi \email{csardi.gabor@gmail.com} }
\references{
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.
}
\seealso{\code{\link[stats]{heatmap}}}
\examples{
library(ALL)
data(ALL)
data(ALLModulesSmall)

if (interactive()) {
  ISA2heatmap(ALLModulesSmall, 1, ALL, norm="feature")
}
}
\keyword{cluster}
