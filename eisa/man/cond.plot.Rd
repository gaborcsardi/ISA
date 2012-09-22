\name{condPlot}
\alias{condPlot}
\title{Plot sample scores of a transcription module}
\description{
  Creates a barplot of sample (=condition) scores, for a given
  transcription module. See details below.
}
\usage{
condPlot (modules, number, eset, col = "white", all = TRUE, sep = NULL, 
    sepcol = "grey", val = TRUE, srt = 90, adj.above = c(0, 0.5),         
    adj.below = c(1, 0.5), plot.only = seq_len(ncol(eset)), \dots)
}
\arguments{
  \item{modules}{An \code{ISAModules} object.}
  \item{number}{An integer scalar, the module to plot.}
  \item{eset}{An \code{ExpressionSet} or \code{ISAExpressionSet}
    object. This is needed for calculating the scores of the samples
    that are not in the module, see the \code{all} argument. If an
    \code{ExpressionSet} object is supplied, then it is normalised by
    calling \code{\link{ISANormalize}} on it.}
  \item{col}{Color of the bars, it it passed to
    \code{\link[graphics]{barplot}}, so it can be any format
    \code{\link[graphics]{barplot}} accepts. E.g. it can be a character
    vector with different colors for the different bars.}
  \item{all}{Logical scalar, whether to plot all samples (if
    \code{TRUE}, the default), or just the ones that are included in the
    module.}
  \item{sep}{\code{NULL} or a numeric vector. If not \code{NULL}, then
    the bars are separated at the given positions with vertical
    lines. This is useful if you want to subdivide the samples into
    groups.}
  \item{sepcol}{The color of the separating lines (see the \code{sep}
    argument), if they are plotted.}
  \item{val}{Logical scalar, whether to add labels with the actual score
    values.}
  \item{srt}{Numeric scalar, the rotation angle of the text labels, this
    is passed to the \code{\link[graphics]{text}} function.}
  \item{adj.above}{Adjustment of the text labels that are above the
    bars. This is passed to \code{\link[graphics]{text}}, see its manual
    for details.}
  \item{adj.below}{Adjustments of the text labels that are below the
    bars. This is passed to \code{\link[graphics]{text}}, see its manual
    for details.}
  \item{plot.only}{Numeric vector, if supplied it is used to plot a
    subset of samples only. By default all samples are plotted.}
  \item{\dots}{Additional argument, to be passed to
    \code{\link[graphics]{barplot}}.}
}
\details{
  \code{condPlot} creates a barplot for the sample scores of an ISA
  transcription module. Each sample is represented as a bar.

  These plots are useful if you want to show that a given transcription
  module separates the samples into two (or more) groups.
  You can assign different colors to the samples, based on some external
  information, e.g. case and control samples can be colored differently.
  
  In most cases the scores are between minus one and one, but this is
  not necessarily true.

  It is possible to assign scores to samples that are not part of the
  module, but this requires performing one (more precisely half) ISA
  iteration step. Currently the function always performs this extra
  step, even if the out-of-module samples are not plotted. Because the
  sample scores in a module are only approximately constant during ISA
  iteration, it might be possible that the plotted scores are slightly
  different than the ones stored in the \code{modules} variable.
}
\value{
  None.
}
\author{
  Gabor Csardi \email{csardi.gabor@gmail.com}
}
\references{
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.
}
\seealso{\code{\link{ISA}} and \code{\link{ISAModules}}.}
\examples{
data(ALLModulesSmall)
library(ALL)
data(ALL)

col <- ifelse(grepl("^B", ALL$BT), "darkolivegreen", "orange")
condPlot(ALLModulesSmall, 1, ALL, col=col)
}
\keyword{cluster}
