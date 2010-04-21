\encoding{UTF-8}
\name{ExportEV}
\alias{ExportEV}
\alias{ExportEV-methods}
\alias{ExportEV,ISAModules-method}
\alias{ExportEV,Biclust-method}
\alias{ExportEV,list-method}
\title{Export an ExpressionView file}
\description{Exports the biclusters identified in  gene expression data
  with all the relevant biological data to an XML file that can be read
  by the ExpressionView Flash applet.} 
\usage{
\S4method{ExportEV}{ISAModules}(biclusters, eset,
         order=OrderEV(biclusters), filename=file.choose(),
         norm=c("sample", "feature", "raw", "x", "y"), cutoff=0.95,
         description=NULL, GO, KEGG, ...)
\S4method{ExportEV}{Biclust}(biclusters, eset, order, filename, norm,
         cutoff, description, ...) 
\S4method{ExportEV}{list}(biclusters, eset, order=OrderEV(biclusters),
         filename=file.choose(),
         norm=c("sample", "feature", "raw", "x", "y"),
         cutoff=0.95, description=NULL, ...) 
}
\arguments{
  \item{biclusters}{An \code{\link[eisa]{ISAModules}} object, a
    \code{\link[biclust]{Biclust}} object, or a named list, the last one
    possibly coming from the \code{isa2} package.} 
  \item{eset}{A \code{\link[Biobase]{ExpressionSet}} object containing
    the gene expression data. Please see below how to use this function
    on other kind of data.}  
  \item{order}{A named list (result of the \code{\link{OrderEV}}
    function) containing the optimal order. If not specified, an
    ordering with the default parameters is performed.} 
  \item{filename}{The filename of the output file. If not specified, the
    file is selected via the user interface.} 
  \item{norm}{The normalization of the gene expression data. The
    \code{\link[isa2]{isa.normalize}} function can normalize (zero mean
    and unit variance) the data with respect to the genes or the
    samples. Possible values: \sQuote{\code{feature}},
    \sQuote{\code{sample}} and \sQuote{\code{raw}}. \sQuote{\code{x}} is
    the same as \sQuote{\code{feature}} and \sQuote{\code{y}} is the same
    as \sQuote{\code{sample}}. The default value is
    \sQuote{\code{sample}}.}
  \item{cutoff}{The cutoff for the coloring is a value between 0 and 1. It
    represents the fraction of data points taken into account for the
    density plots. The default value is 0.95, i.e., the extrema of the
    coloring are chosen in such a way that 95\% of the data points can be
    represented.} 
  \item{description}{A named list containing an alternative description of
    the data. By default, the metadata is extracted from
    \code{eset}. Please see below how to assemble the data description if
    you are dealing with data other than gene expression.}
  \item{GO}{A list of three \code{GOListHyperGResult} objects, containing
    the enrichment calculation results for the three Gene Ontology
    ontologies, for all modules, as returned by the \code{ISAGO} function
    in the \code{eisa} package. If not specified, then it is calculated
    automatically.} 
  \item{KEGG}{A \code{GOListHyperGResult} object, that contains the
    of the enrichment calculation results for all modules, against the
    KEGG pathway database, as returned by the \code{ISAKEGG} function in
    the \code{eisa} package. If not specified, then it is calculated
    automatically.}
  \item{\dots}{Additional arguments, nothing currently.}
}
\details{
  If the data is available in the form of a
  \code{\link[Biobase]{ExpressionSet}}, the \code{ExportEV} function
  automatically uses the metadata associated with the gene expression
  data. If the underlying data does not contain any annotations, you can
  provide them manually, by defining various items in the description
  list, see the second example below.} 
\seealso{\code{\link{OrderEV}}, \code{\link{LaunchEV}},
  \code{\link[eisa]{ISA}}, \code{\link[biclust]{biclust}}} 
\author{Andreas \enc{Lüscher}{Luscher}
  \email{andreas.luescher@a3.epfl.ch}} 
\examples{
## Gene expression data
## We use the acute T-cell lymphocytic leukemia (ALL) data together with
## the Iterative Signature Algorithm (ISA).

## Load the package and the ALL data
library(ExpressionView)
library(eisa)
library(ALL)
library(hgu95av2.db)
data(ALL)

## Initialize random number generator to get reproducible results
set.seed(5)

## Find biclusters (=modules)
## To avoid some minutes of waiting, we just load the data
## set included in the 'eisa' package instead of
## really performing the calculation.
#modules <- ISA(ALL, thr.gene=2.7, thr.cond=1.4)
data(ALLModulesSmall)
modules <- ALLModulesSmall

## Realign the gene exptression matrix to optimize arrangements of
## biclusters 
optimalorder <- OrderEV(modules)

## Export the data to an ExpressionView file
## Don't forget to change the filename
\dontrun{ExportEV(modules, ALL, optimalorder, filename="file.evf")}


## In-silico data
## We use insilico data together with the ISA and manually annotate the
## data set. Simply explore the data file with the Flash applet to
## figure out where the various annotations are placed.

## Load the package
library(ExpressionView)

## Generate noisy in-silico data with dimensions m x n
m <- 50
n <- 500
data <- isa.in.silico(num.rows=m, num.cols=n, noise=0.1,
                      overlap.row=0)[[1]] 

## Find biclusters (=modules)
modules <- isa(data)

## Annotate the rows and columns of data set
rownames(data) <- paste("row", seq_len(nrow(data)))
colnames(data) <- paste("column", seq_len(ncol(data)))

## Add metadata associated with the rows of the data set
rowdata <- outer(1:nrow(data), 1:sample(1:20, 1), function(x, y) {
  paste("row description (", x, ", ", y, ")", sep="")
})
rownames(rowdata) <- rownames(data)
colnames(rowdata) <- paste("row tag", seq_len(ncol(rowdata)))

## Add metadata associated with the columns of the data set 
coldata <- outer(1:ncol(data), 1:sample(1:20, 1), function(x, y) {
  paste("column description (", x, ", ", y, ")", sep="")
})
rownames(coldata) <- colnames(data)
colnames(coldata) <- paste("column tag", seq_len(ncol(coldata)))

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

## Realign the gene exptression matrix to optimize arrangements of
## biclusters 
optimalorder <- OrderEV(modules)

## Export the data to an ExpressionView file
## Don't forget to change the filename
ExportEV(modules, data, optimalorder, filename="file.evf",
         description=description) 
}
\keyword{cluster}
