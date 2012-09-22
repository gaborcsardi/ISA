\name{overlap}
\alias{overlap}
\alias{overlapPlot}
\title{Overlap of ISA biclusters}
\description{
  Plots a network, where each node is a module and modules that overlap
  are closer to each other.
}
\usage{
overlap (modules, algorithm = c("mds", "fr", "drl"), edge.limit = 0.5)
overlapPlot (graph, xsize = 400, ysize = 400, vertex.size = 20,
     vertex.size2 = 10, \dots)
}
\arguments{
  \item{modules}{An \code{\link{ISAModules}} object.}
  \item{algorithm}{The algorithm to use for placing the vertices, a
    character scalar. See details below.}
  \item{edge.limit}{Numeric constant between zero and one, only edges
    between modules that have a Pearson correlation higher than
    \code{edge.limit} will be drawn.} 
  \item{graph}{An \code{igraph} object, as returned by \code{overlap}.}
  \item{xsize}{The width of the plot in pixels, only used to calculate
    the return value, it does not influence the plot itself.}
  \item{ysize}{The height of the plot in pixels, only used to calculate
    the return value, it does not influence the plot itself.}
  \item{vertex.size}{The width of the vertices on the plot.}
  \item{vertex.size2}{The height of the vertices on the plot.}
  \item{\dots}{Additional arguments, these are passed to the
    \code{plot.igraph} function from the \code{igraph} package.}
}
\details{
  An \code{\link{ISAModules}} object may potentially contain many
  modules that overlap. These functions visualize the overlapping
  relationships of a set of modules.

  \code{overlap} creates an \code{igraph} graph with additional
  information on how to plot this graph in a way that nodes representing
  overlapping modules are close to each other.

  \code{overlapPlot} takes such a graph and plots it.

  \code{overlap} can use various algorithms, depending on the
  \code{algorithm} argument. If it is \sQuote{\code{mds}}, then
  multi-dimensional scaling is used, by calling the
  \code{isaMDS} function in the \code{MASS} package.
  If it is \sQuote{\code{fr}}, then
  the Fruchterman-Reingold algorithm is used, through the
  \code{layout.fruchterman.reingold} function of the \code{igraph}
  package. If it is \sQuote{\code{drl}}, then the DrL graph layout
  algorithm is used, see the \code{layout.drl} function
  in the \code{igraph0} package.
}
\value{
  \code{overlap} returns an igraph graph.

  \code{overlapPlot} returns the coordinates of the vertices in a
  two-column matrix, invisibly.
}
\author{ Gabor Csardi \email{Gabor.Csardi@unil.ch} }
\references{
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.  
}
%\seealso{}
\examples{
data(ALLModulesSmall)
G <- overlap(ALLModulesSmall, algorithm="drl", edge.limit=0.3)
if (interactive()) {
  overlapPlot(G)
}
}
\keyword{cluster}
