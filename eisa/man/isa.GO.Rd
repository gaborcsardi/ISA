\name{ISA.GO}
\alias{ISA.GO}
\concept{Gene Ontology}
\concept{Enrichment analysis}
\title{Calculate Gene Ontology enrichment for transcription modules}
\description{
  Gene Ontology enrichment is calculated for each ISA module
  separately. In the end the result is corrected for multiple hypothesis
  testing.
}
\usage{
ISA.GO (modules, org = getOrganism(modules), shortorg = abbreviate(org, 2),
    ann = annotation(modules), features = featureNames(modules),      
    hgCutoff = 0.001, correction = TRUE, correction.method = "holm")      
}
\arguments{
  \item{modules}{An \code{ISAModules} object, a set of ISA modules.}
  \item{org}{Character scalar. The scientific name of the organism, by
    default this is taken from the \code{modules} argument.}
  \item{shortorg}{Character scalar. The two-letter code of the organism,
    this is needed to load the appropriate annotation package. By
    default it is calculated from \code{organism}.}
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
  The Gene Ontology is a database of gene annotation. The annotating
  labels (these are called terms) are standardized and organized into a
  directed acyclic graph. In other words terms may have more specific
  sub-terms, that can have even more specific sub-sub-terms, and so on.

  The Gene Ontology database has three big sub-graphs, the root nodes
  (the most general terms) of these they are the direct children of the
  root term of the whole ontology: biological process, cellular
  component, molecular function. They are usually referred to as
  ontologies.

  The hypergeometric test, a version Fisher's exact test, takes a GO
  term and a gene set (in our case coming from an ISA module) and asks
  whether the number of genes in the set annotated by the term is
  significantly more (or less) than what one would expect by chance.
  
  \code{ISA.GO} performs the hypergeometric test for every module, for
  all GO terms of the three GO ontologies. The GO data is taken from the
  \code{GO.db} package and the organism specific annotation packages:
  \code{org.**.eg.db}, where \sQuote{**} is a two-letter organism code.

  \code{ISA.GO} currently cannot test for under-representation and the
  conditional test, as implemented in the \code{GOstats} package, is not
  available either.
}
\value{A list with three \code{\link{GOListHyperGResult}} objects, for
  the three Gene Ontologies, named
  \item{BP}{aka Biological Processes}
  \item{CC}{aka Cellular Components}
  \item{MF}{aka Molecular Function}
}
\author{ Gabor Csardi \email{Gabor.Csardi@unil.ch} }
\references{
  The Gene Ontology Consortium. Gene ontology: tool for the unification
  of biology. \emph{Nat. Genet.} May 2000;25(1):25-9.
  
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.
}
\seealso{\code{\link{ISA.KEGG}}, \code{\link{ISA.CHR}},
  \code{\link{ISA.miRNA}} for other enrichment calculations.

  The \code{GO.db}, \code{GOstats} and \code{Category} packages.
}
\examples{}
\keyword{cluster}
