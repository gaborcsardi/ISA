\name{gograph}
\alias{gograph}
\alias{gographPlot}
\title{Plot part of the Gene Ontology hierarchy}
\description{These functions help creating a plot of the Gene Ontology
  hierarchy.}
\usage{
gograph (table, colbar.length = 30, label.cex = 1, alpha=1, abbrev=5,
    GOGRAPHS = NULL, go.terms = NULL)
gographPlot (graph, coords = FALSE, \dots)
}
\arguments{
  \item{table}{A data frame with one column, containing the
    \eqn{p}-values of the enriched GO terms. The row names of the data
    frame should contain the GO ids.}
  \item{colbar.length}{Numeric scalar, the length of the color bar.}
  \item{label.cex}{Numeric scalar, factor for the label sizes,
    e.g. \sQuote{2} means double size compared to the default.}
  \item{alpha}{Alpha channel for the fill color of the vertices.}
  \item{abbrev}{Numeric scalar, the minimum length for the abbreviated
    GO ids.}
  \item{GOGRAPHS,go.terms}{These are for internal use only.}
  \item{graph}{An \code{igraph} graph, as returned by the \code{gograph}
    function.}
  \item{coords}{Logical scalar, whether to return the coordinates of the
    vertices on the plot.}
  \item{\dots}{Additional arguments. These are passed to
    \code{plot.igraph}.}
}
\details{
  A GO plot can be created in two steps. \code{gograph} creates an
  \code{igraph} graph object that contains all the information about the
  plot; \code{gographPlot} creates the actual plot.

  The two steps are needed, because \code{gograph} calculates the
  optimal size of the plot, and then a graphics device of this size can
  be created before calling \code{gographPlot}.

  The optimal size is returned by \code{gograph} in the \code{width} and
  \code{height} graph attributes, these can be queried with
  \preformatted{
    G <- gograph(...)
    G$width
    G$height
  }
}
\value{
  \code{gograph} returns an \code{igraph} object.

  \code{gographPlot} by default returns \code{NULL}, invisibly. If the
  \code{coords} argument is \code{TRUE}, then it returns the coordinates
  of the vertices on the plot.
}
\author{ Gabor Csardi \email{Gabor.Csardi@unil.ch} }
\references{
  The Gene Ontology Consortium. Gene ontology: tool for the unification
  of biology. \emph{Nat. Genet.} May 2000;25(1):25-9.
  
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.
}
\seealso{
  The \code{igraph0} package for more about \code{igraph} graphs.
}
\examples{
data(ALLModulesSmall)
GO <- ISAGO(ALLModulesSmall)
gotab <- summary(GO$BP)[[1]][,"Pvalue",drop=FALSE]

G <- gograph(gotab)
if (interactive()) {
  x11(width=G$width/15, height=G$height/15)
  gographPlot(G)
}
}
\keyword{cluster}
