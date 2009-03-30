\name{eisa}
\alias{eisa}
\concept{Iterative Signature Algorithm}
\concept{Biclustering}
\title{Iterative Signature Algorithm on Gene Expression data}
\description{Run ISA on an ExpressionSet with the default parameters.}
\usage{
function (exp.set, flist = filterfun(function(x) IQR(x) > 0.5),            
    uniqueEntrez = TRUE, thr.gene = seq(2, 4, by = 0.5),
    thr.cond = seq(1, 3, by = 0.5), no.seeds = 100)
}
\arguments{
  \item{exp.set}{The input, an ExpressionSet object.}
  \item{flist}{A \sQuote{list} of filter functions to apply to the
    array. This is passed to the \code{\link[genefilter]{genefilter}}
    function without touching it.}
  \item{uniqueEntrez}{Logical scalar, whether to filter the input
    expression set to keep exactly one probeset for each Entrez
    gene. Probets that are not mapped to an Entrez gene are dropped.}
  \item{thr.gene}{Numeric vector. The threshold parameters for the ISA,
    for genes. All combinations of \code{thr.gene} and \code{thr.cond}
    will be used to run ISA.} 
  \item{thr.cond}{Numeric vector. The threshold parameters for the ISA,
    fot conditions. All combinations of \code{thr.gene} and \code{thr.cond}
    will be used to run ISA.}
  \item{no.seeds}{Number of seeds to run ISA from.}
}
\details{
  Please read the \link[isa]{isa-package} manual page in the \code{isa}
  package for an introduction on ISA.

  The \code{eisa} function is a variant of the \code{\link[isa]{isa}}
  function that works on \code{ExpressionSet} objects.
}
\value{
  A named list with the same slots as in the return value of
  \code{\link[isa]{isa}}, except that \sQuote{\code{rows}} is renamed to
  \sQuote{\code{genes}} and \sQuote{\code{columns}} is renamed to
  \sQuote{\code{conditions}}.
}
\author{ Gabor Csardi \email{Gabor.Csardi@unil.ch}}
\references{
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.
  
  Ihmels J, Friedlander G, Bergmann S, Sarig O, Ziv Y, Barkai N:
  Revealing modular organization in the yeast transcriptional network
  \emph{Nat Genet.} 2002 Aug;31(4):370-7. Epub 2002 Jul 22

  Ihmels J, Bergmann S, Barkai N:
  Defining transcription modules using large-scale gene expression data
  \emph{Bioinformatics} 2004 Sep 1;20(13):1993-2003. Epub 2004 Mar 25.
}
\seealso{\code{\link{isa.GO}}, \code{\link{isa.KEGG}},
  \code{\link{isa.CHR}}, \code{\link{isa.miRNA}} for various enrichment
  calculations. See \code{\link[isa]{isa-package}} if you want finer
  control on the ISA run.
}
\examples{}
\keyword{cluster,classif,array,optimize}
