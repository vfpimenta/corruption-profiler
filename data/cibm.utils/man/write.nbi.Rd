\name{write.nbi}
\alias{write.nbi}
\title{Write NBI files}
\usage{
  write.nbi(x, file = NULL, featuresInRows = T,
    indexClass = "last", gzip = T)
}
\arguments{
  \item{x}{dataset}

  \item{file}{string containing path to file}

  \item{featuresInRows}{logical value indicating if
  features are in rows (TRUE) or columns (FALSE)}

  \item{indexClass}{Index of row/column representing class
  labels.  Either an integer or "last" for the last
  row/column. Default is "last".}

  \item{gzip}{logical value indicating if output should be
  compressed in gzip format}
}
\description{
  Writes data matrices to NBI formatted files.
}
\examples{
library("cibm.utils")

data("alzheimer",package="cibm.utils")

alzheimer[c(1:6,121),1:9]

# Write data to NBI formatted file -- Notice by default the file is gzipped
write.nbi(alzheimer,file="alzheimer.nbi",gzip=FALSE)
}
\seealso{
  \code{\link{read.nbi}, \link{write.table}}
}

