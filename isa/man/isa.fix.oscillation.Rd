\name{isa.fix.oscillation}
\alias{isa.fix.oscillation}
\title{Postprocessing oscillating ISA biclusters}
\description{In a set of ISA biclusters, fix the oscillating ones.}
\usage{
isa.fix.oscillation (normed.data, isaresult)
}
\arguments{
  \item{normed.data}{The normalized data matrices that were used to
    calculate the modules.}
  \item{isaresult}{The output of the \code{\link{isa.iterate}}
    function.}
}
\details{
  The current implementation of ISA is able to find oscillating
  biclusters, i.e. biclusters that are not fixed points of the ISA
  oscillation, but that are part of a limit cycle.

  In this case \code{\link{isa.iterate}} stores an arbitrary member
  of the limit cycle in the result. \code{isa.fix.oscillation} checks
  are the modules that are oscillating and makes sure that for a given
  limit cycle always the same element is stored, by picking the one with
  the smallest sum of the row scores.
}
\value{
  A named list with the same entries as the input in \code{isaresult},
  expect that \code{isaresult$rundata$oscillation.fixed} is set to
  \code{TRUE}.
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
\examples{
## An example that is known to have a limit cycle
i <- 2683
while (TRUE) {
while (TRUE) {
i <- i + 1
print(i)
set.seed(i)
# 2192
# 2703

## Create some in silico data, three partially overlapping modules
data <- isa.in.silico(num.fact=3, noise=0.1, overlap.row=2,
                      mod.noise=c(0,0.1,1))
row.seeds <- generate.seeds(length=nrow(data[[1]]), count=100)

## Do ISA with the seeds
normed.data <- isa.normalize(data[[1]])
isaresult <- isa.iterate(normed.data, thr.row=1, thr.col=1,
                         row.seeds=row.seeds, convergence="eps")
 
## There is a single oscillating seed
osc <- which(isaresult$seeddata$oscillation != 0)

## Fix the oscillation
isaresult2 <- isa.fix.oscillation(normed.data, isaresult)
if (any(isaresult2$rows != isaresult$rows)) { break }
}

## Do some iterations and calculate the sum in the vector,
## just to see how it oscillates
row <- isaresult$rows[,osc,drop=FALSE]
osc.sum <- sum(row)
for (j in 2:30) {
  row <- isa:::isa.step(normed.data, rows=row,
                        thr.row=isaresult$seeddata$thr.row[osc],
                        thr.col=isaresult$seeddata$thr.col[osc],
                        direction=isaresult$rundata$direction)
  row <- row$rows
  osc.sum[j] <- sum(row)
}
plot(osc.sum, main=i)
if (length(unique(sign(diff(osc.sum)))) != 1) { break }
}
}
\keyword{cluster,classif,array,optimize}
