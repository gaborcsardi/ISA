\name{isa.option}
\alias{isa.option}
\title{Options for the isa package}
\description{
  This function can be used to set various options that affect many
  functions in the isa package.
}
\usage{
isa.option(\dots)
}
\arguments{
  \item{\dots}{A single option query, or option assignments, these must
    be named, too. See details below.}
}
\details{
  The \code{isa.option} function can be used in three forms. First,
  calling it without any arguments returns a named list of the current
  values of all isa options.

  Second, calling it with a character scalar as the single argument, it
  returns the value of the specified option.

  Third, calling it with a named argument (or more named arguments) set
  the specified options to the given values.

  Here is a list of all the currently supported options:
  \describe{
    \item{\code{verbose}}{Logical scalar. Whether to report what the isa
      functions are currently doing. Defaults to \code{FALSE}.}
    \item{\code{status.function}}{A function object, it serves as a
      callback for printing status messages.}
  }
}
\value{
  In the first form, \code{isa.option} returns a named list with the
  current values of all options.

  In the second form, it returns the value of the specified option.

  In the third form, it returns a named list with the current values of
  all options, invisibly.
}
\author{ Gabor Csardi \email{Gabor.Csardi@unil.ch} }
%\references{}
%\seealso{}
\examples{
## Make isa functions verbose
isa.option(verbose=TRUE)

## Query the value of 'verbose'
isa.option("verbose")

## Query all options
isa.option()
}
\keyword{cluster}
