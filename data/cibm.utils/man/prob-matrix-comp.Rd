\name{pdfDiscrete}
\alias{pdfContinuous}
\alias{pdfDiscrete}
\title{Probability matrix computation}
\usage{
  pdfDiscrete(values, na.zero = TRUE, normalize = TRUE)

  pdfContinuous(values, na.zero = TRUE, normalize = TRUE)
}
\arguments{
  \item{values}{should be an 1-d array}

  \item{na.zero}{a logical value to set if probability of
  NA should be set to zero or left as NA}

  \item{normalize}{a logical value to set if values should
  be scaled such that \eqn{\sum_i prob(i) = 1}}
}
\value{
  array replacing input values by their expected
  probability
}
\description{
  Computes probabilities from data.
}
\author{
  Renato Vimieiro and Carlos Riveros
}

