\name{distance}
\alias{distance}
\title{CIBM distances}
\usage{
  distance(x, method = "JS", save.dist = FALSE,
    col.wise = TRUE, file = NULL)
}
\arguments{
  \item{x}{a dataset to compute the distance}

  \item{method}{the measure to be used. Possible values
  are: JS, cosine, robust, pearson, spearman, kendall,
  cosine cramersV}

  \item{save.dist}{a logical value indicating if the
  distance matrix should be saved to a file}

  \item{col.wise}{a logical value indicating if the
  distance should be computed between columns (TRUE) or
  rows (FALSE)}

  \item{file}{the path of the file where to save the
  distance matrix as RData if \code{save.dist} is TRUE}
}
\value{
  A 'dist' object containing the distance matrix
}
\description{
  Computes a distance matrix using one of the measures
  commonly used in CIBM.  Currently, the measures available
  are the Jensen-Shannon distance, Spearman, Pearson,
  Kendall, the Robust, Cosine, and Cramer's V.
}
\details{
  This function accepts \code{cibm.data}, \code{matrix} and
  \code{data.frame}. Notice that \code{save.dist} will save
  the distance matrix as an \bold{RData} file.
}
\examples{
\dontrun{
data(alzheimer)
dm <- distance(alzheimer)
# Oops...should get an error
# By definition, Jensen-Shannon distance cannot be used with negative values
# This should work now
dm <- distance(alzheimer,"robust")
as.matrix(dm)[1:10,1:10]
}
}
\author{
  Carlos Riveros and Renato Vimieiro
}
\seealso{
  \code{\link[stats]{dist} and \link[cluster]{daisy}} for
  categorical data and other measures
}

