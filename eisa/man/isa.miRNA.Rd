\name{ISAmiRNA}
\alias{ISAmiRNA}
\concept{miRNA}
\concept{Enrichment analysis}
\title{Calculate (predicted) miRNA target enrichment for transcription
  modules}
\description{
  This function performs enrichment calculations with respect to
  predicted miRNA targets to check whether an ISA module contains many
  genes that are targets of the same miRNA.}
\usage{
ISAmiRNA (modules, ann = annotation(modules), features = featureNames(modules), 
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
  miRNAs are short RNA fragments that specifically regulate (usually
  inhibit) the expression of genes. Some genes have been experimentally
  validated as targets of a given miRNA, but we currently don't know the
  target genes of most miRNAs.

  TargetScan is a database of predicted miRNA targets. The predictions are
  done based many factors, including the conservation of the target
  region during evolution.

  The hypergeometric test, a version Fisher's exact test, takes a miRNA
  and a gene set (in our case coming from an ISA module) and asks
  whether the number of genes in the set regulated by the miRNA is
  significantly more (or less) than what one would expect by chance.
  
  \code{ISAmiRNA} performs the hypergeometric test for every module,
  for all miRNAs in the TargetScan database.

  In order to use this function, TargetScan annotation packages are
  needed. These are currently available for Homo Sapiens and Mus
  Musculus and they can be downloaded from
  \url{http://www.unil.ch/cbg/index.php?title=Software}.
}
\value{A \code{\link{miRNAListHyperGResult}} object.}
\author{ Gabor Csardi \email{Gabor.Csardi@unil.ch} }
\references{
  Conserved Seed Pairing, Often Flanked by Adenosines, Indicates that
  Thousands of Human Genes are MicroRNA Targets Benjamin P Lewis,
  Christopher B Burge, David P Bartel. \emph{Cell}, 120:15-20 (2005).  
  
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.
}
\seealso{\code{\link{ISAGO}}, \code{\link{ISAKEGG}} and
  \code{\link{ISACHR}} for other enrichment calculations.

  The \code{Category} package.
}
\examples{
data(ALLModulesSmall)

if (require(targetscan.Hs.eg.db)) {
  miRNA <- ISAmiRNA(ALLModulesSmall)
  summary(miRNA, p=0.1)[[7]]
}
}
\keyword{cluster}
