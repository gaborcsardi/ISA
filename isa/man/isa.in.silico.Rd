\name{isa.in.silico}
\alias{isa.in.silico}
\title{Generate in-silico input data for biclustering algorithms}
\description{}
\usage{
isa.in.silico (num.rows = 300, num.cols = 50, num.fact = 3, fact.per.row = 1,
     fact.per.col = fact.per.row, mod.row.size = round(0.5 * num.rows/num.fact), 
     mod.col.size = round(0.5 * num.cols/num.fact), noise = 0.1,                 
     mod.signal = rep(1, num.fact), mod.noise = rep(0, num.fact),                
     OL = 0)
}
\arguments{
  \item{num.rows}{}
  \item{num.cols}{}
  \item{num.fact}{}
  \item{fact.per.row}{}
  \item{fact.per.col}{}
  \item{mod.row.size}{}
  \item{mod.col.size}{}
  \item{noise}{}
  \item{mod.signal}{}
  \item{mod.noise}{}
  \item{OL}
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
