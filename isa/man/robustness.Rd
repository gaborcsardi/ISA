\name{robustness}
\alias{robustness}
\alias{isa.filter.robust}
\title{Robustness of ISA biclusters}
\description{}
\usage{
robustness (normed.data, row.scores, col.scores)
isa.filter.robust (data, normed.data, isares, perms = 1, row.seeds, col.seeds)
}
\arguments{
  \item{normed.data}{}
  \item{row.scores}{}
  \item{col.scores}{}
  \item{data}{}
  \item{isares}{}
  \item{perms}{}
  \item{row.seeds}{}
  \item{col.seeds}{}
}
\details{}
\value{
  \code{robustness} returns a numeric vector, the robustness score of
  each bicluster.

  \code{isa.filter.robust} returns a named list, the filtered
  \code{isares}, see the return value of \code{\link{isa.iterate}} for
  the structure of the list.
}
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
