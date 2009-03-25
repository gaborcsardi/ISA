\docType{package}
\name{isa-package}
\alias{isa-package}
\alias{ISA}
\title{The isa package}
\description{The Iterative Signature Algorithm}
\section{Introduction}{
  The Iterative Signature Algorithm (ISA) is a biclustering
  algorithm. Biclustering algorithms classify simultaneously the rows
  and columns of an input matrix into biclusters, or as we will call
  them here, modules.
}
\section{ISA biclusters}{
  An ISA module is pair; a subset of the rows of the input matrix and a
  subset of its columns. In other words, a bicluster is a block of the
  reordered input matrix, where reordering means a permutation of both
  the rows and columns. (Another bicluster might be block of the same
  permuted input matrix or one after a different permutation.)

  The criteria of a good bicluster is that 1) its rows are significantly
  different than the other rows, when we consider only the positions
  defined by the columns of the same bicluster, and (symmetrically) 2)
  its columns are significantly different than the other columns, when
  we consider only the positions defined by the rows of the same
  bicluster.

  In other words, the rows of the bicluster are correlated, but only on
  the columns defined by the same bicluster; and the opposite is also
  true, the columns of the bicluster are correlated, but only on the
  rows defined by the same bicluster.

  ISA biclusters are soft, two biclusters may overlap in their 
  rows, columns or even both. It is also possible that some rows and/or
  columns of the input matrix are not found to be part of any ISA
  biclusters. Depending on the stringency parameters, it might even
  happen that ISA does not find any biclusters.
}
\section{How ISA works}{
  ISA works in an iterative way. For an \eqn{E (m\times n)}{E (m x n)}
  input matrix It starts from seed vector \eqn{r_0}{r(0)}, which is
  typically a sparse 0/1 vector of length \eqn{m}. This defines a set of
  rows in \eqn{E}. Then \eqn{E'} is multiplied by \eqn{r_0}{r(0)} and the
  result is thresholded. (Please see also \sQuote{Normalization} below.)

  The thresholding is an important step of the ISA, without thresholding
  ISA would be equivalent to a (not too effective) numerical singular
  value decomposition (SVD). Currently thresholding is done by
  calculating the mean and standard deviation of the vector and keeping
  only elements that are further than a given number of standard
  deviations from the mean. Based on the \code{direction} parameter,
  this means 1) keeping values that are significantly higher than the
  mean (\code{direction=\dQuote{up}}), significantly lower
  (\code{direction=\dQuote{down}}) or both
  (\code{direction=\dQuote{updown}}).
  
  The thresholded vector is the (column) \sQuote{signature} of
  \eqn{r_0}{r(0)}: \eqn{c_0}{c(0)}. Then the signature of \eqn{c_0}{c(0)} is
  calculated, \eqn{E} is multiplied by  \eqn{c_0}{c(0)} and then
  thresholded to calculate \eqn{r_1}{r(1)}.

  The iteration is performed until it converges, i.e. \eqn{r_i}{r(i)}
  and \eqn{r_{i-1}} are \dQuote{close}, and \eqn{c_i}{c(i)} and
  \eqn{c_{i-1}}{c(i-1)} are also close. The convergence criteria,
  i.e. what \dQuote{close} means is by default defined by high Pearson
  correlation. 
}
\section{Random seeding and smart seeding}{
  By default (i.e. if the \code{\link{isa}} function is used) the ISA is
  performed from random sparse starting seeds. This way the algorithm is
  completely unsupervised, but also stochastic: it might give different
  results for different runs.

  It is possible to use non-random seeds as well, if you have some
  knowledge about the data or are interested in a particular subset of
  rows/columns, then you can feed in your seeds into the
  \code{\link{isa.iterate}} function directly. In this case the
  algorithm is deterministic, for the same seed you will always get the
  same results.
}
\section{Normalization}{
  On in silico data we observed that ISA has the best performance if the
  input matrix is normalized (see \code{\link{isa.normalize}}). The
  normalization produces two matrices: \eqn{E_r}{Er} and
  \eqn{E_c}{Ec}. \eqn{E_r}{Er} is calculated by transposing \eqn{E} and
  centering and scaling its rows (see \code{\link{scale}}). \eqn{E_c}{Ec} is
  calculated by centering and scaling the rows of \eqn{E}. \eqn{E_r} is
  used to calculate the column signature of rows and \eqn{E_c} is used
  to calculate the signature of the columns.

  It is possible to 
}
\section{Robustness}{TODO}
\author{Gabor Csardi \email{Gabor.Csardi@unil.ch}}
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
