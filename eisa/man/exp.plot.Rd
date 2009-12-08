\name{expPlot}
\alias{expPlot}
\alias{expPlotCreate}
\alias{expPlotColbar}
\alias{print.ISAexpPlot}
\title{Expression matrix plots for ISA modules}
\description{These functions create an expression matrix plot for an ISA
  module. The gene and sample scores are also plotted.}
\usage{
expPlotCreate (eset, modules, which, norm = c("sample", "raw", "feature"))
expPlot (epo, scores = TRUE)                                              
expPlotColbar (epo)
}
\arguments{
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
  \item{modules}{An \code{\link{ISAModules}} object.}
  \item{which}{Numeric scalar, which module to plot.}
  \item{scores}{Logical scalar, whether to plot the scores as well.}
  \item{epo}{An object returned by \code{expPlotCreate}.}
}
\details{
  \code{expPlotCreate} creates an object that contains all data for
  performing the image plot and returns it. The reason for not plotting
  it directly is, that the size of the plot is usually different in
  different cases, and the opening of the graphics device
  is delayed until \code{expPlotCreate} returns.

  In the returned object, the \code{weight} and \code{height} entries
  give the optimal size of the image, in pixels.

  \code{expPlot} creates the expression plot.

  \code{expPlotColbar} plots a color bar for the expression plot.
}
\value{
  \code{expPlotCreate} returns an \code{ISAexpPlot} object. It is a
  named list and has several entries, the important ones:
  \item{width}{Numeric scalar, the optimal width of the plot.}
  \item{height}{Numeric scalar, the optimal height of the plot.}

  \code{expPlot} returns, invisibly, a named list with members:
  \item{coords}{A list with two entries: \code{x} and \code{y}, both
    numeric vectors of length two. They give the position of the actual
    expression matrix on the plot.}
  \item{gene.width}{Numeric scalar, the width of one box on the image
    plot, in pixels; if the image size is exactly the suggested one.}
  \item{cond.height}{Numeric scalar, the height of one box on the image
    plot, in pixels; if the image size is exactly the suggested one.}
  
  \code{expPlotColbar} returns \code{NULL}, invisibly.
}
\author{ Gabor Csardi \email{Gabor.Csardi@unil.ch} }
\references{
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.
}
\seealso{The vignette in the \code{eisa} package for other ISA
  visualizations. The \code{ExpressionView} package for an interactive
  version.}
\examples{
data(ALLModulesSmall)
library(ALL)
data(ALL)

ep <- expPlotCreate(ALL, ALLModulesSmall, 1)
ep

if (interactive()) {
  expPlot(ep)
}
}
\keyword{cluster}
