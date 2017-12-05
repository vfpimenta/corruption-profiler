\name{kNNCliques}
\alias{kNNCliques}
\title{Clustering based on kNN and Cliques}
\usage{
  kNNCliques(dm, MAX_ITER = 5, basename = "iter",
    format = "graphml", min.size = NULL,
    saveSz = c("all", "top2", "top3", "top"), verbose = T)
}
\arguments{
  \item{dm}{Distance matrix, assumed possitive defined and
  symmetrical}

  \item{MAX_ITER}{Maximum number of iterations to complete}

  \item{basename}{Basename for the graph files written}

  \item{format}{Graphic file format: gml, graphml other as
  supported by igraph}

  \item{min.size}{Min clique size.  If null, set to 2/3 of
  kNN}

  \item{saveSz}{the size of maximum cliques to use: all,
  top, top2, or top3}

  \item{verbose}{logical indicating if progress information
  should be displayed}
}
\description{
  Function to quickly analyse a distance matrix by
  searching cliques on progressively larger kNN islands.
  Results will be saved to file according to
  \code{basename}.

  Graph filenames have the form
  \code{basename_}K\code{nn_}SS\code{_clique_<date>.<format>}
  where K is the number of neighbours in the kNN graph SS
  is the save keyword (top, top2, top3, all)

  K increases with each iteration.
}
\examples{
\dontrun{
# Load dataset
data(alzheimer)
#Compute distance matrix using the robust distance
dm <- distance(alzheimer,method="robust")
#Compute clusters
kNNCliques(dm,format="gml",saveSz="top2")
#Check if files were created
dir(pattern="iter.*")
}
}
\author{
  Carlos Riveros
}
\seealso{
  \code{\link{kmeans}, \link[cluster]{pam}, \link{hclust},
  \link{distance}}
}

