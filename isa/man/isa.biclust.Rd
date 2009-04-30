\name{isa.biclust}
\alias{isa.biclust}
\title{Convert ISA modules to a Biclust class, as defined by the
  biclust package}
\description{
  This function converts the object with ISA modules to a \code{Biclust}
  object, so all the functions in the \code{biclust} package can be used
  on it.
}
\usage{
isa.biclust(modules)
}
\arguments{
  \item{modules}{The ISA modules, as returned by the \code{isa} or some
    other function. }
}
\details{
  \code{biclust} is an R package that implements many biclustering
  algorithms in a unified framework. This function converts a set of ISA
  biclusters to a \code{Biclust} object, this class is used to store all
  biclustering results by the \code{biclust} package.

  The \code{Biclust} class only supports binary biclusters, so the ISA
  modules are binarized during the conversion.
}
\value{
  A \code{Biclust} object.
}
\author{ Gabor Csardi \email{Gabor.Csardi@unil.ch}}
%\references{}
%\seealso{}
\examples{
## You need the biclust package for this
if (require(biclust)) {
  set.seed(1)
  data <- isa.in.silico()
  modules <- isa(data[[1]])
  bc <- isa.biclust(modules)

  ## A heatmap
  drawHeatmap(data[[1]], bc, 1)

  ## A "bubble" plot
  bubbleplot(data[[1]], bc)

  ## Compare values inside and outside the bicluster
  plotclust(bc, data[[1]])

  ## Plot profiles of bicluster elements
  parallelCoordinates(data[[1]], bc, number=1)

  ## Coherence measures vs. ISA robustness
  cV <- sapply(seq(bc@Number), function(x)
     constantVariance(data[[1]], bc, x, dimension="both"))
  aV <- sapply(seq(bc@Number), function(x)
     additiveVariance(data[[1]], bc, x, dimension="both"))
  mV <- sapply(seq(bc@Number), function(x)
     multiplicativeVariance(data[[1]], bc, x, dimension="both"))
  sV <- sapply(seq(bc@Number), function(x)
     signVariance(data[[1]], bc, x, dimension="both"))

  rob <- robustness(isa.normalize(data[[1]]), modules$rows,
     modules$columns)

  cor( cbind(cV, aV, mV, sV, rob) )
}
}