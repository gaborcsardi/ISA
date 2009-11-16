\encoding{UTF-8}
\name{ExportEV}
\alias{ExportEV}
\title{Export ExpressionView file}
\concept{biclustering, gene expression data, interactive}
\description{Exports the biclusters identified in  gene expression data with all the relevant biological data to an XML file that can be read by the ExpressionView Flash applet.}
\usage{
ExportEV(biclusters, eset, order=NULL, filename=NULL, norm=c("sample", "feature", "raw"))
}
\arguments{
\item{biclusters}{An \code{\link[ISAModules]{ISAModules}} object or a \code{\link[Biclust]{Biclust}} object.}
\item{eset}{An \code{\link[ExpressionSet]{ExpressionSet}} object containing the gene expression data.} 
\item{order}{A \code{\link[list]{list}} (result of the \code{\link{OrderEV}} function) containing the optimal order. If not specified, an ordering with the default parameters is performed.}
\item{filename}{The filename of the output file. If not specified, the file is selected via the user interface.}
\item{norm}{The normalization of the gene expression data. The \code{\link[isa.normalize]{isa.normalize}} function can normalize (zero mean and unit variance) the data with respect to the genes or the samples.}
}
\details{
While the ExpressionView Flash applet is optimized for exploring gene expression data, it can also be used to export a \code{\link{data.frame}}: \cr\cr
\code{ExportEV(biclusters, data, order=NULL, filename=NULL, description=NULL)},\cr\cr
where \code{data} is a \code{\link{data.frame}}.
}
\seealso{\code{\link{OrderEV}}, \code{\link{OpenEV}}, \code{\link{ISA}}, \code{\link{biclust}}}
\author{Andreas \enc{Lüscher}{Luscher} \email{andreas.luescher@a3.epfl.ch}}
\examples{
## We apply the Iterative Signature Algorithm (ISA) 
library(eisa)
library(ExpressionView)

library(ALL)
library(hgu95av2.db)
data(ALL)

genethr <- 2.7 
condthr <- 1.4 
set.seed(1)
modules <- ISA(ALL, thr.gene=genethr, thr.cond=condthr)
optimalorder <- order.clusters(modules)

ExportEV(modules, ALL, optimalorder, filename="")
}
\keyword{export, ExpressionView, bicluster, order, isa}
