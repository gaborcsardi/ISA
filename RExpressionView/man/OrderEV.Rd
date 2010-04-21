\encoding{UTF-8}
\name{OrderEV}
\alias{OrderEV}
\alias{OrderEV-methods}
\alias{OrderEV,ISAModules-method}
\alias{OrderEV,Biclust-method}
\alias{OrderEV,list-method}
\title{Find the optimal arrangement of biclusters for visualization in
  ExpressionView}
\description{Finds the optimal arrangement of possibly overlapping
  biclusters that maximizes the areas of the largest contiguous parts of
  the biclusters. The reordering is necessary to obtain a visually
  appealing layout of the biclusters.} 
\usage{
\S4method{OrderEV}{ISAModules}(biclusters, initialorder, maxtime, debuglevel)
\S4method{OrderEV}{Biclust}(biclusters, initialorder, maxtime, debuglevel)
\S4method{OrderEV}{list}(biclusters, initialorder, maxtime, debuglevel)
}
\arguments{
  \item{biclusters}{An \code{\link[eisa]{ISAModules}} object, a
    \code{\link[biclust]{Biclust}} object or a named list. The last one
    is probably coming from the \code{isa2} package.}
  \item{initialorder}{A list containing the initial order. Usually the
    output of a previous ordering.}
  \item{maxtime}{The maximal computation time in seconds. The default
    value is one minute (\code{maxtime=60}).} 
  \item{debuglevel}{The level of information provided during the
    ordering. By default, the debug output is turned off
    (\code{debuglevel=0}).}
}
\details{
  OrderEV performs a brute-force ordering of the biclusters, treating
  the rows and the columns of the matrix independently. The ordering
  algorithm is described in more detail in the accompanying vignette of
  the package.
}
\value{A named list is returned with the following elements:
  \item{rows / genes}{A list containing the maps between the rows of the
    initial and the optimally ordered gene expression matrix. The first
    element represents the map of the complete data set, while the
    subsequent entries contain the row maps of the data sets projected
    onto the individual clusters. This entry is called
    \sQuote{\code{rows}} if the function is called with a simple list as
    the first argument, and \sQuote{\code{genes}} otherwise.} 
  \item{cols / samples}{A list containing the maps between the columns of
    the initial and the optimally ordered gene expression matrix. The
    first element represents the map of the complete data set, while the
    subsequent entries contain the column maps of the data sets
    projected onto the individual clusters. This entry is called
    \sQuote{\code{cols}} if the function is called with a simple list as
    the first argument, and \sQuote{\code{samples}} otherwise.} 
  \item{status}{A list containing the status of the ordering. The list
    has two entries, named \code{genes} and \code{samples} (or
    \code{rows} and \code{cols} if the function was called with a simple
    list as the first argument). Each entry is a numeric vector of ones
    and zeros. A \code{1} indicates that the map is fully optimized,
    whereas a \code{0} signals that the ordering could not be completely
    within the given time frame.}
}
\seealso{\code{\link{ExportEV}}, \code{\link{LaunchEV}},
  \code{\link[eisa]{ISA}}, \code{\link[biclust]{biclust}}}
\author{Andreas \enc{Lüscher}{Luscher}
  \email{andreas.luescher@a3.epfl.ch}}
\examples{
## We generate some noisy in-silico data with biclusters,
## scramble the initially ordered arrangement
## identify the bicluster with the Iterative Signature Algorithm (ISA)
## and order the results with the OrderEV function
library(isa2)
data.in.silico <- isa.in.silico(noise=0.1)[[1]]
data.in.silico <- data.in.silico[sample(c(1:dim(data.in.silico)[1])),
                                 sample(c(1:dim(data.in.silico)[2]))] 
isa.results <- isa(data.in.silico)
optimalorder <- OrderEV(isa.results)
str(optimalorder)

## Create a plot for the scrambled and the optimal orderings
\dontrun{
layout(rbind(1:2))
image(data.in.silico)
image(data.in.silico[optimalorder$rows[[1]],
                     optimalorder$cols[[1]]])
}
}
\keyword{cluster}
