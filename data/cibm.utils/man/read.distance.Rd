\name{read.distance}
\alias{read.distance}
\title{Load NBI files to R}
\usage{
  read.distance(file = NULL)
}
\arguments{
  \item{file}{string containing path to file}
}
\value{
  \item{dist}{containing the distance matrix from the file}
}
\description{
  Loads NBI formatted file containing distance matrix to R.
}
\examples{
# loads library
library("cibm.utils")

# defines filename
filename <- system.file("extdata","alzheimer.nbi",package="cibm.utils")

# reads
alzheimer <- read.nbi(filename)

# prints -- Notice that class is numeric
alzheimer[c(1:6,121),1:9]


}
\seealso{
  \code{\link{read.table}}
}

