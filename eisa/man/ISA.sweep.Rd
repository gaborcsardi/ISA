\name{ISASweep}
\alias{ISASweep}
\alias{ISASweepGraph}
\alias{ISASweepGraphPlot}
\title{Create an ISA module tree}
\description{
  These functions create and plot the hierarchical description of an
  expression data set, by applying the ISA with various thresholds, and
  connecting the related modules. See details below.
}
\usage{
ISASweep (expset, modules, \dots)
ISASweepGraph (sweep.result)
ISASweepGraphPlot (graph, vertex.label=V(graph)$id,
  vertex.label.topleft=NA, vertex.label.topright=NA,
  vertex.label.bottomleft=NA, vertex.label.bottomright=NA,
  vertex.label.cex=0.8, edge.label=NA, asp=FALSE, rescale=FALSE,
  xlim=range(graph$layout[,1]), ylim=range(graph$layout[,2]),
  thresholds=TRUE, xlab=NA, ylab=NA, \dots)
}
\arguments{
  \item{expset}{The expression set object, if it is not an
    \code{ISAExpressionSet}, then \code{ISANormalize} is called on it.}
  \item{modules}{An \code{ISAModules} object.}
  \item{\dots}{Additional arguments. \code{ISASweep} passes these to
    \code{isa.sweep}; \code{ISASweepGraphPlot} passes additional
    arguments to \code{plot.igraph}.}
  \item{sweep.result}{An \code{ISAModules} object that contains the
    sweep tree information as well.}
  \item{graph}{An igraph graph object, the sweep tree.}
  \item{vertex.label}{Vertex labels, by default the ids of the
    modules.}
  \item{vertex.label.topleft}{Vertex labels to put at the top left
    corner.}
  \item{vertex.label.topright}{Vertex labels to put at the top right
    corner.}
  \item{vertex.label.bottomleft}{Vertex labels to put at the bottom left
    corner.}
  \item{vertex.label.bottomright}{Vertex labels to put at the bottom
    right corner.}
  \item{vertex.label.cex}{Magnification factor for the vertex labels.}
  \item{edge.label}{Edge labels.}
  \item{asp}{Logical scalar, whether the plot should have 1:1 aspect
    ratio.}
  \item{rescale}{Logical scalar, whether to rescale the layout
    coordinates to the [-1,1] interval.}
  \item{xlim}{Numeric vector of length two, the X limits of the plot.}
  \item{ylim}{Numeric vector of length two, the Y limits of the plot.}
  \item{thresholds}{Logical scalar, whether to add the (non-constant)
    thresholds to the plot.}
  \item{xlab}{The label of the horizontal axis, by default omitted.}
  \item{ylab}{The label of the vertical axis, by default omitted.}
}
\details{
  The ISA uses two threshold parameters that tune the sizes of the
  transcription modules. The sweep graph of an expression set is defined
  as the following. It is a directed graph, where the vertices are ISA
  modules, found at some threshold parameter values. There is an edge
  from module A to module B, if using 1) (the genes of) module A as the
  seed vector and 2) the threshold parameters used to find module B, the
  ISA converges to module B.
  
  The \code{ISASweep} function creates an ISA sweep tree, in which one
  threshold parameter is kept fixed and the other varies. It starts from
  the modules found at the most stringent (=highest) threshold
  parameters, and uses them individually as seeds at the next less
  stringent threshold level. If this ISA iteration converges to an
  already known module, then an edge of the sweep tree is found. If the
  iteration converges to a new module, then this is added to the module
  list, together with the sweep tree edge. Then we proceed with the next
  level of modules, towards the less stringent threshold parameters.

  The \code{ISASweepGraph} function creates a graph object that
  corresponds to the sweep tree of the expression set.

  The \code{ISASweepGraphPlot} function plots a graph created with
  \code{ISASweepGraph}. 
}
\value{
  \code{ISASweep} returns an \code{ISAModules} object, with some seed
  data added.

  \code{ISASweepGraph} returns an igraph graph object.

  \code{ISASweepGraphPlot} returns \code{NULL}, invisibly.
}
\author{Gabor Csardi \email{csardi.gabor@gmail.com} }
\references{
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.
}
% \seealso{}
\examples{
library(genefilter)
library(ALL)
data(ALL)

varLimit <- 0.5
kLimit <- 4
ALimit <- 5
flist <- filterfun(function(x) var(x)>varLimit, kOverA(kLimit,ALimit))
ALL.filt <- ALL[genefilter(ALL, flist), ]
ALL.filt2 <- ALL.filt[, grepl("^B", ALL.filt$BT)]

# Run ISA
set.seed(2)
modules <- ISA(ALL.filt2, flist=NA, thr.gene=seq(2,4,by=0.5), thr.cond=1)

# Do the sweep
modules2 <- ISASweep(ALL.filt2, modules)
modules2

# Plot it
\dontrun{
  G <- ISASweepGraph(modules2)
  ISASweepGraphPlot(G)
}
}
\keyword{cluster}
