\name{read.abk}
\alias{read.abk}
\title{Load Alfa-Beta-k files to R}
\usage{
  read.abk(file = NULL)
}
\arguments{
  \item{file}{string containing path to file}
}
\value{
  \item{cibm.data}{containing the data from the file}
}
\description{
  Loads Alfa-Beta-k formatted file to R.
}
\examples{
# loads library
library("cibm.utils")

# defines filename
filename <- system.file("extdata","2695.abk",package="cibm.utils")

# reads
a2695 <- read.abk(filename)

# prints -- Notice that class is numeric
a2695[c(1:6,100),1:9]


}
\seealso{
  \code{\link{read.table} \link{write.abk}}
}

