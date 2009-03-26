\name{generate.seeds}
\alias{generate.seeds}
\title{Generate seed vectors for the Iterative Signature Algorithm}
\description{Generate random input seeds for the ISA.}
\usage{
generate.seeds (length, count = 100, method = c("uni"), sparsity=2)
}
\arguments{
  \item{length}{The length of the seeds, should be the number of rows in
    your input data for row seeds and the number of columns for column
    seeds.}
  \item{count}{The number of seeds to gnerate.}
  \item{method}{The method for generating the seeds. Currently only
    \code{"uni"} is supported, that picks the 1 elements in each seed
    uniformly randomly.}
  \item{sparsity}{A numeric scalar, an integer number giving the number
    of non-zero values in each seed vector. It will be recycled to have
    the same length as the number of seeds.}
}
\details{
  This function can generate a 0/1 matrix whose columns are the seeds of
  the ISA. The result can be use as the \code{row.seeds} (or
  \code{col.seeds}) argument of the \code{\link{isa.iterate}} function.
}
\value{
  A numeric matrix with 0/1 values.
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
## Just to get always the same result
set.seed(24)

## Create some random seeds with different sparseness
data <- isa.in.silico()
sparsity <- rep( c(1,5,25,125), length=100)
row.seeds <- generate.seeds(length=nrow(data[[1]]), count=100,
                            sparsity=sparsity)

## Do ISA with the seeds
normed.data <- isa.normalize(data[[1]])
isaresult <- isa.iterate(normed.data, thr.row=1, thr.col=1,
                         row.seeds=row.seeds)

## Add the sparsity to the seed data
isaresult$seeddata$sparsity <- sparsity

## Check which ones leed to higher robustness scores
rob <- robustness(normed.data, isaresult$rows, isaresult$columns)
tapply(rob, sparsity, mean)
## About the same

## How many unique modules did we find for the different sparsity
isaresult.unique <- isa.unique(normed.data, isaresult)
tapply(seq_len(ncol(isaresult.unique$rows)),
       isaresult.unique$seeddata$sparsity, length)
## We usually find more modules with sparser seeds
}
\keyword{cluster,classif,array,optimize}
