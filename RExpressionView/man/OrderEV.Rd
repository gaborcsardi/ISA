\encoding{UTF-8}
\name{OrderEV}
\alias{OrderEV}
\title{Find optimal arrangement of biclusters for visualization in ExpressionView}
\concept{biclustering, gene expression data, interactive}
\description{Finds the optimal arrangement of possibly overlapping biclusters that maximizes the areas of the largest contiguous parts of the biclusters. The reordering is necessary to obtain a visually appealing layout of the biclusters.}
\usage{
OrderEV(biclusters, initialorder=NULL, maxtime=60, debuglevel=0)
}
\arguments{
\item{biclusters}{An \code{\link[ISAModules]{ISAModules}} object, a \code{\link[Biclust]{Biclust}} object or a \code{list}.}
\item{initialorder}{A list containing the initial order. Usually the output of a previous ordering.}
\item{maxtime}{The maximal computation time in seconds. The default value is one minute (\code{maxtime=60}).}
\item{debuglevel}{The level of information provided during the ordering. By default, the debug output is turned off (\code{debuglevel=0}).}
}
\details{}
\value{A named list is returned with the following elements:
\item{rows / samples}{A list containing the maps of the rows of the gene expression matrix. The first element represents the map of the complete data set, while the subsequent entries contain the row maps of the data set projected onto the individual clusters.}
\item{cols / genes}{A list containing the maps of the columns of the gene expression matrix. The first element represents the map of the complete data set, while the subsequent entries contain the column maps of the data set projected onto the individual clusters.}
\item{status}{A list containing the status of the ordering. A \code{1} indicates that the map is fully optimized, whereas a \code{0} signals an not yet completely optimized map.}
}
\seealso{\code{\link{ExportEV}}, \code{\link{OpenEV}}, \code{\link{ISA}}, \code{\link{biclust}}}
\author{Andreas \enc{Lüscher}{Luscher} \email{andreas.luescher@a3.epfl.ch}}
\examples{
## We generate some noisy in-silico data with biclusters,
## scramble the initially ordered arrangement
## identify the bicluster with the Iterative Signature Algorithm (ISA)
## and order the results with the OrderEV function
data.in.silico <- isa.in.silico(noise=0.1)[[1]]
data.in.silico <- data.in.silico[sample(c(1:dim(data.in.silico)[1])),sample(c(1:dim(data.in.silico)[2]))]
isa.results <- isa(data.in.silico)
optimalorder <- OrderEV(isa.results)
str(optimalorder)
}
\keyword{order, ExpressionView, bicluster, order, isa}
