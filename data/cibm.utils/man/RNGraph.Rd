\name{RNGraph}
\alias{RNGraph}
\title{Constructs Relative Neighbour Graph based on distance}
\usage{
  RNGraph(dm)
}
\arguments{
  \item{dm}{Distance matrix, assumed possitive defined and
  symmetrical}
}
\value{
  An \code{igraph} graph containing the RNG
}
\description{
  Function to construct the Relative Neighbour Graph from a
  distance matrix. No assumption of geometry implied, It is
  only required that the matrix be symmetric and positive
  definite.  Diagonal is ignored.
}
\examples{
\dontrun{
# Load dataset
data(alzheimer)
#Compute distance matrix using the robust distance
dm <- distance(alzheimer,method="robust")
#Compute graph
gr1 <- RNGraph(dm)
#Compute JS distance
cdata <- apply(alzheimer[],2,pdfContinuous)
dm <- distance(cdata)
gr2 <- RNGraph(dm)
}
}
\author{
  Carlos Riveros
}
\seealso{
  \code{\link{kmeans}, \link[cluster]{pam}, \link{hclust},
  \link{distance}}
}

