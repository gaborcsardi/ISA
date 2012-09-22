\name{ISACHR}
\alias{ISACHR}
\concept{Chromosome}
\concept{Enrichment analysis}
\title{Calculate chromosome enrichment for transcription modules}
\description{
  Hypergeometric test(s) to check whether significantly many genes of an
  ISA module are on the same chromosome.
}
\usage{
ISACHR (modules, ann = annotation(modules), features = featureNames(modules), 
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
  The hypergeometric test, a version Fisher's exact test, takes a
  chromosome and a gene set (in our case coming from an ISA module) and
  asks whether the number of genes in the set that are on the given
  chromosome is significantly more (or less) than what one would expect
  by chance.
  
  \code{ISACHR} performs the hypergeometric test for every module, for
  every chromosome. The chromosome mapping is taken from the annotation
  package of the chip.
  
  \code{ISACHR} currently cannot test for under-representation.
}
\value{
  A \code{\link{CHRListHyperGResult}} object.
}
\author{ Gabor Csardi \email{csardi.gabor@gmail.com} }
\references{
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.
}
\seealso{
  \code{\link{ISAGO}}, \code{\link{ISAKEGG}} and
  \code{\link{ISAmiRNA}} for other enrichment calculations.

  The \code{Category} package.
}
\examples{
data(ALLModulesSmall)
CHR <- ISACHR(ALLModulesSmall)
CHR
sigCategories(CHR)[[2]]
geneIdsByCategory(CHR)[[2]][[1]]
}
\keyword{cluster}
