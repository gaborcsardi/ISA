\name{ISAKEGG}
\alias{ISAKEGG}
\concept{Enrichment analysis}
\title{Calculate KEGG Pathway enrichment for transcription modules}
\description{
  KEGG pathway enrichment is calculated for each ISA module
  separately. In the end the result is corrected for multiple hypothesis
  testing.
}
\usage{
ISAKEGG (modules,ann = annotation(modules), features = featureNames(modules), 
    hgCutoff = 0.05, correction = TRUE, correction.method = "holm") 
}
\arguments{
  \item{modules}{An \code{ISAModules} object, a set of ISA modules.}
  \item{ann}{Character scalar. The annotation package to be used. By
    default it is taken from the \code{modules} argument.}
  \item{features}{Character vector. The names of the features. By
    default it is taken from the \code{modules} argument.}
  \item{hgCutoff}{Numeric scalar. The cutoff value to be used for the
    enrichment significance. This can be changed later, without
    recalculating the test.}
  \item{correction}{Logical scalar, whether to perform multiple
    hypothesis testing correction.}
  \item{correction.method}{Character scalar, the multiple testing
    correction method to use. Possible values: \dQuote{holm},
    \dQuote{hochberg}, \dQuote{hommel}, \dQuote{bonferroni},
    \dQuote{BH}, \dQuote{BY}, \dQuote{fdr}, \dQuote{none}. See the
    \code{\link[stats]{p.adjust}} function for details on these. }
}
\details{
  KEGG (Kyoto Encyclopedia of Genes and Genomes) is a collection of
  online databases dealing with genomes, enzymatic pathways, and
  biological chemicals. The PATHWAY database records networks of
  molecular interactions in the cells, and variants of them specific to
  particular organisms.

  The hypergeometric test, a version Fisher's exact test, takes a KEGG
  pathway and a gene set (in our case coming from an ISA module) and asks
  whether the number of genes in the set participating in the pathway, is
  significantly more (or less) than what one would expect by chance.
  
  \code{ISAKEGG} performs the hypergeometric test for every module, for
  all KEGG pathways. The KEGG data is taken from the \code{KEGG.db}
  package and the annotation package of the chip.

  \code{ISAKEGG} currently cannot test for under-representation.
}
\value{A \code{\link{KEGGListHyperGResult}} object.}
\author{ Gabor Csardi \email{Gabor.Csardi@unil.ch} }
\references{
  \url{http://www.genome.jp/kegg/}

  Kanehisa M, Goto S, Kawashima S, Okuno Y, Hattori M., The KEGG
  resource for deciphering the genome, \emph{Nucleic Acids Res.} 2004
  Jan 1;32(Database issue):D277-80.
  
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.
}
\seealso{\code{\link{ISAGO}}, \code{\link{ISACHR}},
  \code{\link{ISAmiRNA}} for other enrichment calculations.

  The \code{KEGG.db} and \code{Category} packages.
}
\examples{
data(ALLModulesSmall)
KEGG <- ISAKEGG(ALLModulesSmall)
KEGG
sigCategories(KEGG)[[1]]
summary(KEGG)[[1]][,1:5]
}
\keyword{cluster}
