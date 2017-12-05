\name{mstknn}
\alias{mstknn}
\title{Recursive MSTkNN clustering}
\usage{
  mstknn(d, k = NULL, min.size = 20, verbose = FALSE)
}
\arguments{
  \item{d}{a matrix or 'dist' object with the distance
  matrix for the computation.  Should be symmetrical and
  positive definite, although this is not checked nor
  enforced.  Column names, if present, are used to label
  vertices, otherwise they are imposed as \code{vNNNN},
  where NNNN is the row/column number.}

  \item{k}{an optional number of neighbors to compute the
  kNN graph.  The default is to compute as
  \code{max{ceil(log(|V|)), k \ kNN is not connected}}.}

  \item{min.size}{Minimum cluster size.  Clusters smaller
  than this size will not be recursively search for split.}

  \item{verbose}{If messages are desired.}
}
\value{
  A graph object is returned.
}
\description{
  Runs the MSTkNN iteratively splitting up the clusters
  based on the k prescription
}
\details{
  The distance is used to construct a Minimum Spanning Tree
  and k-Nearest Neighbors graphs. All edges in MST not in
  kNN are removed. The process is repeated recursively for
  each resulting connected component, until no more changes
  in the graph structure occur. The initial number of
  neighbors can be provided externally.  By default, it is
  estimated as \code{min{floor(log(|V|)), k \ kNN is
  connected}}.

  \bold{TODO:} "Collection" of small clusters (2 - 3 nodes)
  into larger clusters based on removed MST edge.
}
\examples{
a <- matrix(data=runif(20000),nrow=200)
colnames(a) <- sapply(1:dim(a)[2], function(x) paste("v",x,sep=""))
d <- distance(a)
gmstknn <- mstknn(d,verbose=T)
clusters(gmstknn)
\dontrun{write.graph(gmstknn,file="test.gml",format="gml")}

# Another example with the Ray et al. Alzheimer's data
data(alzheimer)
d <- distance(alzheimer,method="pearson")
gmstknn <- mstknn(d)
clusters(gmstknn)
\dontrun{write.graph(gmstknn,file="test.gml",format="gml")}
}
\seealso{
  \code{\link[igraph::write.graph]{write.graph}},\code{\link[igraph::clusters]{clusters},
  \link[cibm.utils::mstknn]{mstknn}}
}

