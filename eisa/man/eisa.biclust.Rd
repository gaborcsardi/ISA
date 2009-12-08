\name{ISA-Biclust conversion}
\alias{coerce,Biclust,ISAModules-method}
\alias{coerce,ISAModules,Biclust-method}
\concept{Biclust package}
\title{Convert ISA modules to a Biclust object, or the opposite}
\description{The biclust package implements several biclustering
  algorithms in a unified framework. The result of the biclustering is a
  \code{\link[biclust]{Biclust}} object. These functions allow the
  conversion between \code{\link[biclust]{Biclust}} and
  \code{\link{ISAModules}} objects.
}
\details{
  To convert an \code{ISAModules} object (\code{mods}) to a
  \code{Biclust} object (\code{bc}), you can do:
  \preformatted{
    bc <- as(mods, "Biclust")
  }
  The seed data and run data of the \code{ISAModules} object is stored
  in the \code{Parameters} slot of the \code{Biclust} object. The ISA
  scores are binarized by the conversion.

  To convert a \code{Biclust} object (\code{bc}) to an \code{ISAModules}
  object (\code{mods}), you can call:
  \preformatted{
    mods <- as(bc, "ISAModules")
  }
  The \code{Parameters} slot of the \code{Biclust} object is used as the
  run data of the \code{ISAModules} object. The seed data of the new
  object will be an empty data frame. We suggest that the used adds the
  name of the appropriate annotation package to the \code{Biclust}
  object, before converting it to an \code{ISAModules} object. See an
  example on how to do this below.
  
}
\author{ Gabor Csardi \email{Gabor.Csardi@unil.ch} }
\references{
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.

  Sebastian Kaiser, Rodrigo Santamaria, Roberto Theron, Luis Quintales
  and Friedrich Leisch. (2009). biclust: BiCluster Algorithms. R
  package version 0.8.1. http://CRAN.R-project.org/package=biclust  
}
%\seealso{}
\examples{
if (require(biclust)) {

  library(ALL)
  data(ALL)
  ALL.filtered <- ALL[sample(1:nrow(ALL), 1000),]

  # Biclust -> ISAModules
  set.seed(1)
  Bc <- biclust(exprs(ALL.filtered), BCPlaid(),
                fit.model = ~m + a + b, verbose = FALSE)
  Bc@Parameters$annotation <- annotation(ALL)
  modules <- as(Bc, "ISAModules")
  Bc
  modules
  getNoFeatures(modules)
  getNoSamples(modules)

  # ISAModules -> Biclust
  data(ALLModulesSmall)
  Bc2 <- as(ALLModulesSmall, "Biclust")
  ALLModulesSmall
  getNoFeatures(ALLModulesSmall)
  getNoSamples(ALLModulesSmall)
  Bc2

}
}
\keyword{cluster}
