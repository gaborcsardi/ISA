\name{isa.iterate}
\alias{isa.iterate}
\title{The Iterative Signature Algorithm}
\description{}
\usage{
isa.iterate (normed.data, row.seeds, col.seeds, thr.row, thr.col = thr.row,    
     direction = c("updown", "updown"), convergence = c("cor",               
         "loosy", "eps"), cor.limit = 0.99, eps = 1e-04, oscillation = TRUE, 
     maxiter = 100)
}
\arguments{
  \item{normed.data}{}
  \item{row.seeds}{}
  \item{col.seeds}{}
  \item{thr.row}{}
  \item{thr.col}{}
  \item{direction}{}
  \item{convergence}{}
  \item{cor.limit}{}
  \item{eps}{}
  \item{oscillation}{}
  \item{maxiter}{}
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
