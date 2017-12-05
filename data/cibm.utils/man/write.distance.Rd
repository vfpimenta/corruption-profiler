\name{write.distance}
\alias{write.distance}
\title{Write distance matrices in NBI format}
\usage{
  write.distance(dm, file = NULL, gzip = T)
}
\arguments{
  \item{dm}{A distance object or matrix}

  \item{file}{string containing path to file}

  \item{gzip}{logical value indicating if output should be
  compressed in gzip format}
}
\description{
  Write distance matrices in NBI format
}
\examples{
\dontrun{
x <- data.frame(S1=1:4,S2=2:5,S3=3:6,row.names=letters[1:4])
dm <- dist(x)
write.distance(dm,"test.distance.nbi",gzip=F) # by default gzip=T
}
}
\seealso{
  \code{\link{read.distance}, \link{write.nbi}}
}

