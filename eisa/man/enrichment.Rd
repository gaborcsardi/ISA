\name{enrichment}
\alias{anrichment}
\alias{ISAEnrichment}
\concept{Enrichment analysis}
\title{Enrichment analysis for transcription modules, based on
  user-defined categories} 
\description{
  This function performs enrichment analysis for each ISA module
  separately, comparing it to user-defined categories. It is useful to
  test against other databases and annotations than the Gene Ontology
  or KEGG pathways.
}
\usage{
}
\arguments{
  \item{modules}{An \code{ISAModules} object, a set of ISA modules.}
  \item{categories}{}
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
}
\value{
}
\author{ Gabor Csardi \email{Gabor.Csardi@unil.ch} }
\references{
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.  
}
\seealso{
  \code{\link{ISAGO}}, \code{\link{ISACHR}}, \code{\link{ISAKEGG}} and
  \code{\link{ISAmiRNA}} for other enrichment calculations.

  The \code{Category} package.
}
\examples{
data(ALLModulesSmall)
entrez <- unique(unlist(mget(featureNames(ALLModulesSmall), hgu95av2ENTREZID)))
categories <- lapply(letters, function(x) sample(entrez, 100))
names(categories) <- letters
fakeEnrichment1 <- ISAEnrichment(ALLModulesSmall, categories, correction=FALSE)
fakeEnrichment2 <- ISAEnrichment(ALLModulesSmall, categories, correction=TRUE)
}
\keyword{cluster}
