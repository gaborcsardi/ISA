\name{mnplot}
\alias{mnplot}
\alias{ISAmnplot}
\title{Plot group means against each other, for an ISA module}
\description{Plot mean expression values for two sets of samples,
  against each other.}
\usage{
mnplot (x, expset, group, \dots)
ISAmnplot (modules, number, eset, norm = c("raw", "feature", "sample"),
     group, \dots)
}
\arguments{
  \item{x}{A character vector, the feature names for which the plot is
    created.}
  \item{expset}{An \code{ExpressionSet} object (\code{Biobase} package),
    or an expression matrix, with row names as feature names.}
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
  \item{group}{A factor that defines two groups to plot one against the
    other.}
  \item{modules}{An \code{\link{ISAModules}} object.}
  \item{number}{A numeric scalar, the number of the module for which the
    plot is created.}
  \item{\dots}{Additional arguments, they are passed to the
    \code{\link[graphics]{plot}} function.}
}
\details{
  \code{mnplot} plots two group-means against each other, the mean
  expression of all the specified probes. The two groups are specified
  as a factor with two levels.

  \code{ISAmnplot} calls \code{mnplot} and plots the mean expression of
  genes in an ISA module, again, for two groups.
}
\value{
  Both functions return invisibly a matrix with two lines, the mean
  expression values for the two groups, for all the specified genes.
}
\author{ Gabor Csardi \email{Gabor.Csardi@unil.ch} }
\references{
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.
}
\seealso{The \code{\link[annotate]{GOmnplot}} and
  \code{KEGGmnplot} functions in the \code{annotate}
  package.}
\examples{
data(ALLModulesSmall)
library(ALL)
data(ALL)
group <- ifelse(grepl("^B", ALL$BT), "B-cell", "T-cell")
ISAmnplot(ALLModulesSmall, 2, ALL, norm="feature", group=group)
}
\keyword{cluster}
