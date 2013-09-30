\name{ISAModules-class}
\docType{class}
\alias{ISAModules}
\alias{class:ISAModules}
\alias{ISAModules-class}
\alias{seedData}
\alias{seedData,ISAModules-method}
\alias{runData}
\alias{runData,ISAModules-method}
\alias{featureNames,ISAModules-method}
\alias{sampleNames,ISAModules-method}
\alias{getFeatures}
\alias{getFeatures,ISAModules-method}
\alias{getSamples}
\alias{getSamples,ISAModules-method}
\alias{getFeatureNames}
\alias{getFeatureNames,ISAModules-method}
\alias{getSampleNames}
\alias{getSampleNames,ISAModules-method}
\alias{getFeatureMatrix}
\alias{getFeatureMatrix,ISAModules-method}
\alias{getSampleMatrix}
\alias{getSampleMatrix,ISAModules-method}
\alias{getFullFeatureMatrix}
\alias{getFullFeatureMatrix,ISAModules-method}
\alias{getFullSampleMatrix}
\alias{getFullSampleMatrix,ISAModules-method}
\alias{getFeatureScores}
\alias{getFeatureScores,ISAModules-method}
\alias{getSampleScores}
\alias{getSampleScores,ISAModules-method}
\alias{getNoFeatures}
\alias{getNoFeatures,ISAModules-method}
\alias{getNoSamples}
\alias{getNoSamples,ISAModules-method}
\alias{featureThreshold}
\alias{featureThreshold,ISAModules-method}
\alias{sampleThreshold}
\alias{sampleThreshold,ISAModules-method}
\alias{length,ISAModules-method}
\alias{dim,ISAModules-method}
\alias{pData,ISAModules-method}
\alias{annotation,ISAModules-method}
\alias{getOrganism}
\alias{getOrganism,ISAModules-method}
\alias{[,ISAModules,ANY-method}
\alias{[,ISAModules,ANY,ANY-method}
\alias{[,ISAModules,ANY,ANY,ANY-method}
\alias{[[,ISAModules,ANY-method}
\alias{[[,ISAModules,ANY,ANY-method}
\title{A set of ISA modules}
\description{
  An \code{ISAModules} object stores the results of one ISA run. It
  contains a set of biclusters (=modules or transcription modules) and
  some meta information about the ISA run and the input data.
}
\usage{
\S4method{dim}{ISAModules}(x)
\S4method{featureNames}{ISAModules}(modules)
\S4method{sampleNames}{ISAModules}(modules)
\S4method{annotation}{ISAModules}(modules)
\S4method{getOrganism}{ISAModules}(modules)
\S4method{pData}{ISAModules}(modules)

\S4method{seedData}{ISAModules}(modules)
\S4method{runData}{ISAModules}(modules)
\S4method{featureThreshold}{ISAModules}(modules, mods)
\S4method{sampleThreshold}{ISAModules}(modules, mods)

\S4method{length}{ISAModules}(x)
\S4method{getNoFeatures}{ISAModules}(modules, mods)
\S4method{getNoSamples}{ISAModules}(modules, mods)

\S4method{getFeatures}{ISAModules}(modules, mods)
\S4method{getSamples}{ISAModules}(modules, mods)
\S4method{getFeatureNames}{ISAModules}(modules, mods)
\S4method{getSampleNames}{ISAModules}(modules, mods)
\S4method{getFeatureScores}{ISAModules}(modules, mods)
\S4method{getSampleScores}{ISAModules}(modules, mods)
\S4method{getFeatureMatrix}{ISAModules}(modules, binary = FALSE,
      sparse = FALSE, mods)
\S4method{getSampleMatrix}{ISAModules}(modules, binary = FALSE,
      sparse = FALSE, mods)
\S4method{getFullFeatureMatrix}{ISAModules}(modules, eset, mods)
\S4method{getFullSampleMatrix}{ISAModules}(modules, eset, mods)

\S4method{[}{ISAModules,ANY,ANY}(x, i, j, \dots, drop = FALSE)
\S4method{[[}{ISAModules,ANY,ANY}(x, i, j, \dots, drop = FALSE)
}
\arguments{
  \item{x,modules}{An \code{ISAModules} object.}
  \item{mods}{An optional numeric index vector for the modules. If given,
    the information is only returned only for the specified modules.}
  \item{binary}{Logical scalar. Whether to binarize the feature or
    sample scores.} 
  \item{sparse}{Logical scalar. Whether to return a sparse matrix. The
    \code{Matrix} package is required for sparse matrices.}
  \item{eset}{An \code{ExpressionSet} or \code{ISAExpressionSet}
    object. This is needed for calculating the scores of the
    features/samples that are not in the module.
    If an \code{ExpressionSet} object is supplied, then it is
    normalised by calling \code{\link{ISANormalize}} on it.}
  \item{i}{For \sQuote{\code{[}} an index vector for selecting features
    (=probes, genes). For \sQuote{\code{[[}} an index vector for
    selecting modules.}
  \item{j}{For \sQuote{\code{[}} an index vector for selecting
    samples. It is ignored for \sQuote{\code{[[}}.}
  \item{\dots}{Additional indexing arguments, they are not used, just
    ignored.}
  \item{drop}{This argument is currently not used, just silently
    ignored.}
}
\details{
  An \code{ISAModules} object contains a set of biclusters, obtained
  using one run of the Iterative Signature Algorithm.

  Various operations are defined such an object, here we document all of
  them, in several groups.
}
\section{Information about the input data.}{
  \code{dim} returns the dimension of the input expression matrix,
  number of features times number of samples.

  \code{featureNames} returns a character vector, the names of the
  features in the original input matrix. I.e. in the input was an
  \code{ExpressionSet} for an Affymetrix array, then the Affymetrix
  probe IDs are returned.

  \code{sampleNames} returns a character vector, the names of the
  samples in the original expression set.

  \code{annotation} returns a character scalar, the name of the array
  for the input expression set. More precisely, the \code{annotation}
  slot of the input \code{ExpressionSet} is returned, this is the name
  of the annotation package to use for the \code{ExpressionSet}.

  \code{getOrganism} returns the scientific name of the organism for
  which the input expression data was measures. This is obtained by
  loading the annotation package of the input \code{ExpressionSet}
  object, so that must be installed.

  \code{pData} returns the phenotypic data attached to the input
  \code{ExpressionSet} object, in a data frame, samples as rows and
  various phenotypic variables as columns.
}
\section{Information about the ISA run}{
  \code{seedData} returns information about the modules. Each row of the
  returned data frame corresponds to one module, the columns are various
  variables:
  \describe{
    \item{iterations}{The number of ISA iterations needed to find the module.}
    \item{oscillation}{The length of the oscillation cycle for
       oscillating modules, zero for others.}
    \item{thr.row}{The feature (=gene) threshold used for finding the module.}
    \item{thr.col}{The sample (=condition) threshold used for finding
       the module.}
    \item{freq}{The number of times the module was found. This is always
       one, unless \code{\link{ISAUnique}} was performed.}
    \item{rob}{The robustness score of the module. See
       \code{\link{ISARobustness}} for details.}
    \item{rob.limit}{The robustness limit that was used for filtering
       the modules. As this depends of the feature and sample
       thresholds, it may be different for different modules.}
  }

  \code{runData} returns information about the ISA runs, it is a named
  list with elements:
  \describe{
    \item{annotation}{The annotation package corresponding to the input
      expression set.}
    \item{organism}{The scientific name of the organism.}
    \item{direction}{The \code{direction} parameter of the ISA. Please
       see \code{\link{ISAIterate}} for details.}
    \item{convergence}{The method to determine ISA convergence, a
       character scalar. Please see \code{\link{ISAIterate}} for
       details.}
    \item{cor.limit}{Correlation limit for the \dQuote{\code{cor}}
       convergence criterium, see \code{\link{ISAIterate}} for
       details.}
    \item{eps}{Difference limit for the \dQuote{\code{eps}} convergence
       criterium, see \code{\link{ISAIterate}} for details.}
    \item{corx}{Size of the time window for the \dQuote{\code{corx}}
       convergence criterium, see \code{\link{ISAIterate}} for
       details.}
    \item{maxiter}{The maximum number of ISA iterations that was
       allowed.}
    \item{oscillation}{Logical, whether oscillating modules were
       considered during the ISA iteration.}
    \item{N}{Numeric scalar, the number of input seeds that were used to
       find the modules.}
    \item{unique}{Logical scalar, whether \code{\link{ISAUnique}} was
       run on the modules.}
    \item{prenormalize}{Logical scalar, whether the input data was
       prenormalized during ISA normalization, see
       \code{\link{ISANormalize}}.}
    \item{hasNA}{Logical scalar, whether the normalized input data
       contained some \code{NA} or \code{NaN} values.}
    \item{rob.perms}{Numeric scalar, the number of times the input data
       was scrambled when the modules were filtered according to robustness.}
  }
  Note that some of these might be missing, i.e. \code{rob.perms} is
  only present if \code{\link{ISAFilterRobust}} was performed.

  \code{featureThreshold} returns the feature thresholds that were used
  to find the modules.

  \code{sampleThreshold} returns the sample thresholds that were used to
  find the modules.
}
\section{Information about the modules}{
  \code{length} returns the number of modules.

  \code{getNoFeatures} returns the number of features (=genes) in the
  input data. The number of features \emph{after} filtering is returned
  if the input data was filtered.

  \code{getNoSamples} returns the number of samples (=conditions) in the
  input data.
}
\section{Retrieve the modules}{
  \code{getFeatures} returns the indices of the features included in the
  modules. It returns a list, with one entry for each module. Each entry
  contains the indices of the features (=genes) in the corresponding
  module.

  \code{getSamples} does the same as \code{getFeatures}, but for samples.

  \code{getFeatureNames} is similar to \code{getFeatures}, but returns
  feature names instead of feature indices.

  \code{getSampleNames} is similar to \code{getSamples}, but returns
  sample names instead of sample indices.

  \code{getFeatureScores} returns the feature scores for the selected
  modules (all modules by default). It returns a list, with one entry
  for each module. Each list entry contains the feature scores for one
  module, in a named numeric vector.

  \code{getSampleScores} is similar to \code{getFeatureScores}, but for
  samples and sample scores.

  \code{getFeatureMatrix} returns feature scores for the specified
  modules (all modules by default) in a matrix form. The number of rows
  is the number of features and the number of columns is the number of
  modules requested. It can optionally binarize the values.

  \code{getSampleMatrix} is similar to \code{getFeatureMatrix}, but for
  sample scores.

  \code{getFullFeatureMatrix} is similar to \code{getFeatureMatrix}, but
  is also calculates scores for the features that were not included in
  the module. For this it performs one ISA iteration and omits the
  thresholding step. You need to supply the normalized (or the original)
  expression data to make this possible.

  \code{getFullSampleMatrix} is the same as \code{getFullFeatureMatrix},
  but for sample scores.
}
\section{Indexing}{
  A couple of indexing operations were defined to make it easier
  selecting subsets of modules, features or samples from an
  \code{ISAModules} object.

  The \sQuote{\code{[[}} double bracket indexing operator can be used
  with a single index vector to select a subset of modules.

  The \sQuote{\code{[}} single bracket indexing operator can be used to
  restrict an \code{ISAModules} object to a subset of features and/or
  samples. The first index corresponds to features, the second to
  samples. Indices can be numeric, logical or character vectors, for the
  latter feature and sample names are used.
}
\value{
  \code{dim} returns a numeric vector of length two.
  \code{featureNames} and \code{sampleNames} return a character vector.
  \code{annotation} and \code{getOrganism} return a character vector of length one.
  \code{pData} returns a data frame.

  \code{seedData} returns a data frame, see more below.
  \code{runData} returns a named list, see more below.
  \code{featureThreshold} and \code{sampleThreshold} return a numeric
  vector.

  \code{length} returns a numeric scalar.
  \code{getNoFeatures} and \code{getNoSamples} return a numeric vector.

  \code{getFeatures} and \code{getSamples} return a list of named
  numeric vectors. \code{getFeatureNames} and \code{getSampleNames}
  return a list of character vectors. \code{getFeatureScores} and
  \code{getSampleScores} return a list of named numeric
  vectors. \code{getFeatureMatrix}, \code{getSampleMatrix},
  \code{getFullFeatureMatrix} and \code{getFullSampleMatrix} return a
  numeric matrix.
}
\author{ Gabor Csardi \email{csardi.gabor@gmail.com} }
\references{
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.
}
\seealso{
  The vignette included in the \code{eisa} package.
}
\examples{
data(ALLModulesSmall)
ALLModulesSmall

length(ALLModulesSmall)
dim(ALLModulesSmall)
annotation(ALLModulesSmall)
getOrganism(ALLModulesSmall)

seedData(ALLModulesSmall)

getNoFeatures(ALLModulesSmall)
getNoSamples(ALLModulesSmall)

getFeatureScores(ALLModulesSmall, 1)[[1]]
}
\keyword{cluster}
