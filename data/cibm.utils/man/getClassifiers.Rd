\name{getClassifiers}
\alias{getClassifiers}
\title{List of available Weka classifiers}
\usage{
  getClassifiers(regex = NULL)
}
\arguments{
  \item{regex}{A list of regular expressions. Default value
  is NULL, which makes function return full list of
  available classifiers.}
}
\value{
  \code{character} An array of Weka classes matching
  \emph{regex}.
}
\description{
  Returns a list of Weka classes that match the given
  regular expression or the full list if regex is not
  present.
}
\examples{
# Full list
getClassifiers()
# All trees and bayes classifiers
getClassifiers(c("trees","bayes"))
# Getting a specific classifier
getClassifiers("J48$")
}

