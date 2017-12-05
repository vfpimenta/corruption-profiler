\name{write.abk}
\alias{write.abk}
\title{Write discrete matrix in Alfa-Beta-k format}
\usage{
  write.abk(x, file = NULL, featuresInRows = T,
    classes = "last", out.equalweights = F, gzip = T)
}
\arguments{
  \item{x}{An object of type \code{cibm.abk}}

  \item{file}{string containing path to file}

  \item{featuresInRows}{To specify major file format
  (ignored, only TRUE supported)}

  \item{classes}{To specify where do class labels get
  written. One of 'first', 'last', 'none'. If none, then no
  classes will be written even if present.}

  \item{out.equalweights}{Normally, if all weights are
  equal they are not written to file.}

  \item{gzip}{logical value indicating if output should be
  compressed in gzip format}
}
\description{
  Write discrete matrix in Alfa-Beta-k format
}
\examples{
\dontrun{
x <- data.frame(S1=1:4,S2=2:5,S3=3:6,row.names=letters[1:4])
write.abk(x,"test.abk",gzip=F) # by default gzip=T
}
}
\seealso{
  \code{\link{read.abk}, \link{write.nbi}}
}

