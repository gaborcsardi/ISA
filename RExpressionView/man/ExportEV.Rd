\encoding{UTF-8}
\name{ExportEV}
\alias{ExportEV}
\alias{ExportEV-methods}
\alias{ExportEV,ISAModules-method}
\alias{ExportEV,Biclust-method}
\alias{ExportEV,list-method}
\title{Export an ExpressionView file}
\concept{biclustering, gene expression data, interactive}
\description{Exports the biclusters identified in  gene expression data with all the relevant biological data to an XML file that can be read by the ExpressionView Flash applet.}
\usage{
\S4method{ExportEV}{ISAModules}(biclusters, eset, order, filename, norm, description)
\S4method{ExportEV}{Biclust}(biclusters, eset, order, filename, norm, description)
\S4method{ExportEV}{list}(biclusters, eset, order, filename, norm, description)
}
\arguments{
\item{biclusters}{An \code{\link[eisa]{ISAModules}} object, a \code{\link[biclust]{Biclust}} object, or a named list.}
\item{eset}{A \code{\link[Biobase]{ExpressionSet}} object containing the gene expression data.} 
\item{order}{A named list (result of the \code{\link{OrderEV}} function) containing the optimal order. If not specified, an ordering with the default parameters is performed.}
\item{filename}{The filename of the output file. If not specified, the file is selected via the user interface.}
\item{norm}{The normalization of the gene expression data. The \code{\link[isa2]{isa.normalize}} function can normalize (zero mean and unit variance) the data with respect to the genes or the samples. The default value is c("sample", "feature", "raw").}
\item{description}{A named list containing the description of the data.}
}
\details{
If the data is available in the form of a \code{\link[Biobase]{ExpressionSet}}, the \code{ExportEV} function automatically uses the metadata associated with the gene expression data. If the underlying data does not contain any annotations, you can provide them manually, by defining various items in the description list, see the second example below.}
\seealso{\code{\link{OrderEV}}, \code{\link{LaunchEV}}, \code{\link[eisa]{ISA}}, \code{\link[biclust]{biclust}}}
\author{Andreas \enc{Lüscher}{Luscher} \email{andreas.luescher@a3.epfl.ch}}
\examples{
## Gene expression data
## We use the acute T-cell lymphocytic leukemia (ALL) data together with the 
## Iterative Signature Algorithm (ISA).
library(ExpressionView)
library(ALL)
library(hgu95av2.db)
data(ALL)

set.seed(5)
modules <- ISA(ALL, thr.gene=2.7, thr.cond=1.4)
optimalorder <- OrderEV(modules)
ExportEV(modules, ALL, optimalorder, filename="")


## In-silico data
## We use insilico data together with the ISA
## and manually annotate the data set.
library(ExpressionView)


}
\keyword{export, ExpressionView, bicluster, order, isa}
