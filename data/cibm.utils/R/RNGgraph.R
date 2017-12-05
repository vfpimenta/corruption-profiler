#########################################################################################
#   CENTRE FOR BIOINFORMATICS, BIOMARKER-DISCOVERY & INFORMATION-BASED MEDICINE
#   THE UNIVERSITY UNIVERSITY OF NEWCASTLE
#   University Drive, Callaghan, NSW, 2308, AUSTRALIA
#
#   Created on: 2013/07/16
#   Author: Carlos Riveros
#   Modified by:
#                          
#   License: MIT <http://opensource.org/licenses/MIT>
#
#   Copyright (c) 2013 Carlos Riveros
#

#' @name RNGraph
#' @title Constructs Relative Neighbour Graph based on distance
#' @author Carlos Riveros
#' @description Function to construct the Relative Neighbour Graph from a distance matrix.
#' No assumption of geometry implied, It is only required that the matrix be symmetric
#' and positive definite.  Diagonal is ignored.
#'              
#' @param dm Distance matrix, assumed possitive defined and symmetrical
#' @seealso \code{\link{kmeans}, \link[cluster]{pam}, \link{hclust}, \link{distance}}
#' @return An \code{igraph} graph containing the RNG
#' @import igraph
#' @export
#' 
#' @examples
#' \dontrun{
#' # Load dataset
#' data(alzheimer)
#' #Compute distance matrix using the robust distance
#' dm <- distance(alzheimer,method="robust")
#' #Compute graph
#' gr1 <- RNGraph(dm)
#' #Compute JS distance
#' cdata <- apply(alzheimer[],2,pdfContinuous)
#' dm <- distance(cdata)
#' gr2 <- RNGraph(dm)
#' }
RNGraph <- function(dm) {
    am <- .RNGadjacency(dm)
    gr <- graph.adjacency(am,mode="upper")
    if ((is.null(V(gr)$label) || length(V(gr)$label) == 0) &&
        (is.null(V(gr)$name) || length(V(gr)$name) == 0)) {
        warning("No labels provided.  Node labels will be numbers")
        V(gr)$name <- as.character(seq(1,dim(am)[1]))
    }
    else if (is.null(V(gr)$label) || length(V(gr)$label) == 0) {
        V(gr)$label <- V(gr)$name    
    }
    else if (is.null(V(gr)$name) || length(V(gr)$name) == 0) {
        V(gr)$name <- V(gr)$label
    }
    return(gr)
}

.RNGadjacency <- function(dm) {
    # Object must be coerced to matrix
    dm = as.matrix(dm)
    d <- dim(dm)
    if (d[1] != d[2] || d[1] == 0 || any(dm < 0))
        stop("Data must be a squared, positive definite distance matrix")
    N <- d[1]
    # ordered accessors
    oo <- t(apply(dm+diag(Inf,ncol=N,nrow=N),1,order))
    # Adjacency matrix, sort of (negative values are used as temporary values)
    am <- matrix(0, nrow=N, ncol=N)
    colnames(am) <- colnames(dm)
    rownames(am) <- colnames(dm)

    # First neighbour is always in RNG
    for (j in 1:N) {
        am[j,oo[j,1]] <- 1
        am[oo[j,1],j] <- 1
    }
    # Loop on columns
    for (j in 2:(N-1)) {
        # Loop on rows
        for (k in 1:N) {
            if (am[k,j] != 0)
                next
            cr <- oo[k,j]           # we check edge (k,cr)
            cc <- match(k,oo[cr,])  # position of k in neighbour list for cr
            # is any neighbour of k closer than cr also a neighbour of cr closer than k ?
            uu <- oo[k,1:(j-1)] %in% oo[cr,1:(cc-1)]
            # print(paste(k,cr," in ",uu))
            if (any(uu)) {
                am[k,cr] <- -1  # no edge, checked
                am[cr,k] <- -1
            }
            else {
                am[k,cr] <- 1
                am[cr,k] <- 1
            }
        }
        # Signal no new edge this ranked neighbour
        uu <- FALSE
        for (k in 1:N)
            uu <- uu | am[k,oo[k,j]] > 0
        # print(paste("Column ",j,uu))
        if (!uu)
            break
    }
    am[am < 1] <- 0
    return(am)
}

