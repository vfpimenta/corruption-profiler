\name{read.nbi}
\alias{read.nbi}
\title{Load NBI files to R}
\usage{
  read.nbi(file = NULL)
}
\arguments{
  \item{file}{string containing path to file}
}
\value{
  \item{cibm.data}{containing the data from the file}
}
\description{
  Loads NBI formatted file to R.
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
\author{
  Renato Vimieiro

  Carlos Riveros
}
\seealso{
  \code{\link{read.table}}
}

