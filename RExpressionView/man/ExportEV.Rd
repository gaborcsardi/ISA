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
\S4method{ExportEV}{ISAModules}(biclusters, eset, order, filename, norm,
         cutoff, description, GO, KEGG, ...)
\S4method{ExportEV}{Biclust}(biclusters, eset, order, filename, norm,
         cutoff, description, ...) 
\S4method{ExportEV}{list}(biclusters, eset, order, filename, norm,
         cutoff, description, ...) 
}
\arguments{
\item{biclusters}{An \code{\link[eisa]{ISAModules}} object, a \code{\link[biclust]{Biclust}} object, or a named list.}
\item{eset}{A \code{\link[Biobase]{ExpressionSet}} object containing the gene expression data.} 
\item{order}{A named list (result of the \code{\link{OrderEV}} function) containing the optimal order. If not specified, an ordering with the default parameters is performed.}
\item{filename}{The filename of the output file. If not specified, the file is selected via the user interface.}
\item{norm}{The normalization of the gene expression data. The \code{\link[isa2]{isa.normalize}} function can normalize (zero mean and unit variance) the data with respect to the genes or the samples. The default value is c("feature", "sample", "raw") or alternatively c("x", "y", "raw").}
\item{cutoff}{The cutoff for the coloring is a value between 0 and 1. It represents the fraction of data points taken into account for the density plots. The default value is 0.95, i.e., the extrema of the coloring are chosen in such a way that 95\% of the data points can be represented.}
\item{description}{A named list containing an alternative description of
  the data. By default, the metadata is extracted from eset.}
\item{GO}{A list of three \code{GOListHyperGResult} objects, containing
  the enrichment calculation results for the three Gene Ontology
  ontologies, for all modules, as returned by the \code{ISAGO} function
  in the \code{eisa} package.} 
\item{KEGG}{A \code{GOListHyperGResult} object, that contains the
  of the enrichment calculation results for all modules, against the
  KEGG pathway database, as returned by the \code{ISAKEGG} function in
  the \code{eisa} package.}
\item{\dots}{Additional arguments, nothing currently.}
}
\details{
If the data is available in the form of a \code{\link[Biobase]{ExpressionSet}}, the \code{ExportEV} function automatically uses the metadata associated with the gene expression data. If the underlying data does not contain any annotations, you can provide them manually, by defining various items in the description list, see the second example below.}
\seealso{\code{\link{OrderEV}}, \code{\link{LaunchEV}}, \code{\link[eisa]{ISA}}, \code{\link[biclust]{biclust}}}
\author{Andreas \enc{Lüscher}{Luscher} \email{andreas.luescher@a3.epfl.ch}}
\examples{
## Gene expression data
## We use the acute T-cell lymphocytic leukemia (ALL) data together with the 
## Iterative Signature Algorithm (ISA).

## Load the package and the ALL data
library(ExpressionView)
library(eisa)
library(ALL)
library(hgu95av2.db)
data(ALL)
## Initialize random number generator to get reproducible results
set.seed(5)
## Find biclusters (=modules)
modules <- ISA(ALL, thr.gene=2.7, thr.cond=1.4)
## Realign the gene exptression matrix to optimize arrangements of biclusters
optimalorder <- OrderEV(modules)
## Export the data to an ExpressionView file
## Don't forget to change the filename
ExportEV(modules, ALL, optimalorder, filename="file.evf")


## In-silico data
## We use insilico data together with the ISA and manually annotate the data set.
## Simply explore the data file with the Flash applet to figure out where the 
## various annotations are placed.

## Load the package
library(ExpressionView)
## Generate noisy in-silico data with dimensions m x n
m <- 50
n <- 500
data <- isa.in.silico(num.rows=m, num.cols=n, noise=0.1, overlap.row=0)[[1]]
## Find biclusters (=modules)
modules <- isa(data)
## Annotate the rows and columns of data set
v <- vector("character")
for ( i in 1:dim(data)[1] ) { 
	v[i] <- paste("row ", i, sep="")
}; 
rownames(data) <- v
v <- vector("character")
for ( i in 1:dim(data)[2] ) { 
	v[i] <- paste("column ", i, sep="")
}
colnames(data) <- v
## Add metadata associated with the rows of the data set
rowdata <- matrix(nrow=dim(data)[1], ncol=round(runif(1, 1, 20)))
rownames(rowdata) <- rownames(data)
v <- vector("character")
for ( i in 1:dim(rowdata)[2] ) { 
	v[i] <- paste("row tag ", i, sep="")
}
colnames(rowdata) <- v
for ( i in 1:dim(rowdata)[1] ) { 
	for ( j in 1:dim(rowdata)[2] ) { 
		rowdata[i,j] <- paste("row description (", i, ", ", j, ")", sep="")
	}
}
## Add metadata associated with the columns of the data set
coldata <- matrix(nrow=dim(data)[2], ncol=round(runif(1, 1, 20)))
rownames(coldata) <- colnames(data)
v <- vector("character")
for ( i in 1:dim(coldata)[2] ) { 
	v[i] <- paste("column tag ", i, sep="")
}
colnames(coldata) <- v
for ( i in 1:dim(coldata)[1] ) { 
	for ( j in 1:dim(coldata)[2] ) { 
		coldata[i,j] <- paste("column description (", i, ", ", j, ")", sep="")
	}
}
## Merge the different annotations in a single list and 
## add a few global things
description <- list(
experiment=list(
	title="Title", 
	xaxislabel="x-Axis Label",
	yaxislabel="y-Axis Label",
	name="Author", 
	lab="Address", 
	abstract="Abstract", 
	url="URL", 
	annotation="Annotation", 
	organism="Organism"),
coldata=coldata,
rowdata=rowdata
)
## Realign the gene exptression matrix to optimize arrangements of biclusters
optimalorder <- OrderEV(modules)
## Export the data to an ExpressionView file
## Don't forget to change the filename
ExportEV(modules, data, optimalorder, filename="file.evf", description=description)
}
\keyword{export, ExpressionView, bicluster, order, isa}
