\name{isa.sweep}
\alias{isa.sweep}
\alias{sweep.graph}
\title{Create a hierarchical structure of ISA biclusters}
\description{}
\usage{
isa.sweep (data, normed.data, isaresult, method = c("cor"), neg.cor = TRUE,  
     cor.limit = 0.9)
sweep.graph (sweep.result)
}
\arguments{
  \item{data}{}
  \item{normed.data}{}
  \item{isaresult}{}
  \item{method}{}
  \item{neg.cor}{}
  \item{cor.limit}{}
}
\details{}
\value{}
\author{ Gabor Csardi \email{Gabor.Csardi@unil.ch} }
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
\seealso{ \link{isa-package} for a short introduction on the Iterative
  Signature Algorithm. See \code{\link{isa}} for an easy way of running
  ISA.
}
\examples{}
\keyword{cluster,classif,array,optimize}
