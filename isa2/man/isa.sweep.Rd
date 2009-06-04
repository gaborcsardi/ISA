\name{isa.sweep}
\alias{isa.sweep}
\alias{isa.sweep,matrix-method}
\alias{sweep.graph}
\alias{sweep.graph,list-method}
\title{Create a hierarchical structure of ISA biclusters}
\description{
  Relate the biclusters found in many ISA runs on the same input data.
}
\usage{
\S4method{isa.sweep}{matrix}(data, \dots)
\S4method{sweep.graph}{list}(sweep.result, \dots)
}
\arguments{
  \item{data}{The input matrix.}
  \item{\dots}{Additional arguments, see details
    below. \code{sweep.graph} has no additional arguments currently.}
  \item{sweep.result}{An ISA result with hierarchy information in the
    seed data, typically calculated by the \code{isa.sweep} function.}
}
\details{
  \code{isa.sweep} can be called as
  \preformatted{
    isa.sweep(data, normed.data, isaresult,
              method = c("cor"), neg.cor = TRUE,  
	      cor.limit = 0.9)
	    }
  where the arguments are:	    
  \describe{
    \item{data}{The input matrix.}
    \item{normed.data}{The normalized input matrix, usually the output of
      the \code{\link{isa.normalize}} function.}
    \item{isaresult}{An object containing the biclusters, the result of
      \code{\link{isa}} or \code{\link{isa.iterate}}.}
    \item{method}{Character scalar giving the method to determine which
      seed converged which bicluster. Right now only \sQuote{\code{cor}}
      is supported, this is based on Pearson correlation.}
    \item{neg.cor}{Logical scalar, whether to consider negative
      correlation as convergence.}
    \item{cor.limit}{Numeric scalar giving the minimum correlation for
      convergence.}
  }

  Many ISA runs with different thresholds typically create a bunch of
  biclusters and it is useful to visualize how these are related.

  From a set of biclusters for which of the \code{thr.row} and
  \code{thr.col} parameters was the same, but the other was not,
  \code{isa.sweep} creates a hierarchy of modules.

  The hierarchy is a directed graph of modules in which every node has
  an out degree at most one. An edge pointing from module \eqn{m} to
  module \eqn{n} means that module \eqn{n} is \dQuote{part of} module
  \eqn{m}; in the sense that an ISA iteration started from module
  \eqn{n} converges to module \eqn{m} at the (milder) thresholds of
  module \eqn{m}.

  The information about the module relationships is stored in a column
  of the seed data.

  \code{sweep.graph} takes the output of \code{isa.sweep} and creates a
  graph object of it. For this the \sQuote{igraph} package is required
  to be installed on the system.
}
\value{
  \code{isa.sweep} returns a named list with the same components as in
  the input (\code{isaresult}), but the \sQuote{\code{father}} and the
  \sQuote{\code{level}} columns are
  added to the \sQuote{\code{seeddata}} member. \code{father} contains
  the edges of the sweep graph: if bicluster \eqn{m} is the father of
  bicluster \eqn{n} that means that bicluster \eqn{n} converges to
  bicluster \eqn{m} at the same threshold parameters that were used to
  find biclusters \eqn{m}.

  \code{level} is a simple numbering of the different thresholds for
  which the sweep tree was built. I.e. the most strict threshold is
  level one, the second most is level two, etc.

  \code{sweep.graph} returns and igraph graph.
}
\author{ Gabor Csardi \email{Gabor.Csardi@unil.ch} }
\references{
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.
  
  Ihmels J, Friedlander G, Bergmann S, Sarig O, Ziv Y, Barkai N:
  Revealing modular organization in the yeast transcriptional network
  \emph{Nat Genet.} 2002 Aug;31(4):370-7. Epub 2002 Jul 22

  Ihmels J, Bergmann S, Barkai N:
  Defining transcription modules using large-scale gene expression data
  \emph{Bioinformatics} 2004 Sep 1;20(13):1993-2003. Epub 2004 Mar 25.
}
\seealso{ \link{isa2-package} for a short introduction on the Iterative
  Signature Algorithm. See \code{\link{isa}} for an easy way of running
  ISA.
}
\examples{
## In-silico data
set.seed(1)
insili <- isa.in.silico()

## Do ISA with a bunch of row thresholds while keeping the column
## threshold fixed. This is quite an artificial example...
isares <- isa(insili[[1]], thr.row=c(0.5,1,2), thr.col=0)

## Create a nice tree from the modules, we need the normed data for this
nm <- isa.normalize(insili[[1]])
isa.tree <- isa.sweep(insili[[1]], nm, isares)
network <- sweep.graph(isa.tree)

## Plot the network of modules, only if a recent igraph package is
## installed
if (interactive() && require(igraph) &&
    compareVersion(packageDescription("igraph")$Version, "0.6")>=0) {
  lab <- paste(sep="", seq_len(ncol(isa.tree$rows)), ": ",
               colSums(isa.tree$rows!=0), ",",
               colSums(isa.tree$columns!=0))
  par(mar=c(1,1,1,1))
  roots <- tapply(topological.sort(network, mode="out"),
                  clusters(network)$membership, function(x) x[1])
  rootlevel <- isa.tree$seeddata$level-1
  coords <- layout.reingold.tilford(network, root=roots,
                                    rootlevel=rootlevel[roots+1])
  plot(network, layout=coords,
       vertex.shape="rectangle", vertex.color="green",
       vertex.label=lab, vertex.size=30, vertex.size2=10)
}

## Plot the modules themselves as well
if (interactive()) {
  plotModules(isa.tree)
}

## Yet another plot, the scores for the rows within the modules
if (interactive()) {
  layout(matrix( 1:15, nc=3 ))
  for (i in seq(ncol(isa.tree$rows))) {
    par(mar=c(2,2,1,1))
    plot(isa.tree$rows[,i], axes=FALSE, ylim=c(-1,1))
    axis(1); axis(2)
    text(nrow(isa.tree$rows), 1, adj=c(1,1), paste(sep="", "#", i), cex=2)
  }
}
    
}
\keyword{cluster}
