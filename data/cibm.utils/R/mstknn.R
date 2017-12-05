#########################################################################################
#   CENTRE FOR BIOINFORMATICS, BIOMARKER-DISCOVERY & INFORMATION-BASED MEDICINE
#   THE UNIVERSITY UNIVERSITY OF NEWCASTLE
#   University Drive, Callaghan, NSW, 2308, AUSTRALIA
#
#   Contains utility functions frequently used in CIBM
#
#   Created on: 2014/02/27
#   Last modified on: 2014/04/03
#   author: Carlos Riveros
#   updated: Renato Vimieiro
#                          
#   License: MIT <http://opensource.org/licenses/MIT>
#
#   Copyright (c) 2014 Carlos Riveros
#
#  [20140403:RV] Added attribute label to IGRAPH object to make it display
#                the label in yEd.
#
#  [20150316:RV] Replaced graph.difference.by.name with graph.difference
#                as the new igraph package renamed the function and removed
#                the first from the library

# @title MST-kNN clustering
# @name mstknn
# @rdname mstknn
# @author Carlos Riveros
# @description Main step of clustering via the MST-kNN algorithm from (reference).
# @details The distance is used to construct a Minimum Spanning Tree and a k-Nearest Neighbors
# graphs. All edges in MST not in kNN are removed. The process is repeated recursively for each 
# resulting connected component, until no more changes in the graph structure occur.
# The initial number of neighbors can be provided externally. By default, it is estimated as 
# \code{ceil(|V|)}.
# 
#              A graph object is returned.
#              
# \bold{TODO:} "Collection" of small clusters (2 - 3 nodes) into larger clusters based on removed
# MST edge.
# @export
# @param d a matrix or 'dist' object with the distance matrix for the computation.  SHould be 
#        symmetrical and positive definite, although this is not checked nor enforced.
#        Column names, if present, are used to label vertices.
# @param k an optional number of neighbors to compute the kNN graph.  The default is 
#        to compute as \code{max{ceil(log(|V|)), k \ kNN is not connected}}. 
# @param verbose If messages are desired.
# @seealso \code{\link[stats]{dist}, \link[cluster]{daisy}} and \code{\link["cibm.utils"]{JSD}} 
#          for categorical data and other measures
# @return A graph object with the MST-kNN clusters.  Edge weights are distances.
# @import igraph Matrix
# @examples
# 
# a <- matrix(data=runif(20000),nrow=200)
# colnames(a) <- sapply(1:dim(a)[2], function(x) paste("v",x,sep=""))
# d <- distance(a)
# gmstknn <- mstknn(d)
# \dontrun{
# plot(gmstknn,layout=layout.reingold.tilford(gmst))
# write.graph(gmstknn,file="random.cluster.gml",format="gml")
# }


.mstknn <- function(d,k=NULL,verbose=FALSE) {
    tini <- Sys.time()
    nc <- dim(d)[1]
    iter <- is.null(k)
    if(iter)
        k <- floor(log(nc))
    
    # Construct Minimum Spanning Tree
    ai <- Sys.time()
    gmst <- minimum.spanning.tree(
        graph.adjacency(d,mode='undirected',weighted=TRUE),
        algorithm='prim'
        )
    ae <- Sys.time()
    if(verbose) {
        message(sprintf('  MST: time=%fs',difftime(ae,ai,units="secs")))
        summary(gmst)
    }
    
    # Adjacency matrix for MST
    mmst <- get.adjacency(gmst,attr='weight')
    
    r <- .kNNadjacency(d,k,iter,verbose)
    
    mmst <- mmst * r$adjacency
    gmst <- graph.adjacency(mmst,mode='undirected',weighted=TRUE)
    tend <- Sys.time()
    if(verbose) {
        message(sprintf('  MSTkNN: time=%fs',difftime(tend,tini,units="secs")))
        summary(gmst)
    }
    return(gmst)
}


#' @title       Recursive MSTkNN clustering
#' @name        mstknn
#' @description Runs the MSTkNN iteratively splitting up the clusters based 
#'              on the k prescription
#' @details     The distance is used to construct a Minimum Spanning Tree and k-Nearest Neighbors
#'              graphs. All edges in MST not in kNN are removed. The process is repeated 
#'              recursively for each resulting connected component, until no more changes in the 
#'              graph structure occur. The initial number of neighbors can be provided externally. 
#'              By default, it is estimated as 
#'              \code{min{floor(log(|V|)), k \ kNN is connected}}.      
#'              
#'              \bold{TODO:} "Collection" of small clusters (2 - 3 nodes) into larger clusters 
#'              based on removed MST edge.
#' @return      A graph object is returned.
#' @export
#' @param d     a matrix or 'dist' object with the distance matrix for the computation.  Should be 
#'              symmetrical and positive definite, although this is not checked nor enforced.
#'              Column names, if present, are used to label vertices, otherwise they are imposed as
#'              \code{vNNNN}, where NNNN is the row/column number.
#' @param k     an optional number of neighbors to compute the kNN graph.  The default is 
#'              to compute as \code{max{ceil(log(|V|)), k \ kNN is not connected}}. 
#' @param min.size Minimum cluster size.  Clusters smaller than this size will not be 
#'              recursively search for split.
#' @param verbose If messages are desired.
#' 
#' @seealso \code{\link[igraph::write.graph]{write.graph}},\code{\link[igraph::clusters]{clusters}, \link[cibm.utils::mstknn]{mstknn}}
#' @examples
#' 
#' a <- matrix(data=runif(20000),nrow=200)
#' colnames(a) <- sapply(1:dim(a)[2], function(x) paste("v",x,sep=""))
#' d <- distance(a)
#' gmstknn <- mstknn(d,verbose=T)
#' clusters(gmstknn)
#' \dontrun{write.graph(gmstknn,file="test.gml",format="gml")}
#' 
#' # Another example with the Ray et al. Alzheimer's data 
#' data(alzheimer)
#' d <- distance(alzheimer,method="pearson")
#' gmstknn <- mstknn(d)
#' clusters(gmstknn)
#' \dontrun{write.graph(gmstknn,file="test.gml",format="gml")}

mstknn <- function(d,k=NULL,min.size=20,verbose=FALSE) {
    if(class(d) == 'dist') d <- as(d, "matrix")
    if(min.size < 4) min.size <- 4
    stopifnot(dim(d)[1] == dim(d)[2])
    nc <- dim(d)[1]
    if(is.null(colnames(d))) 
        colnames(d) <- sapply(1:nc, function(x) paste("v",x,sep=""))
    if(is.null(rownames(d)) || any(rownames(d) != colnames(d)))
        rownames(d) <- colnames(d)
    
    # Adding attribute label to vertices because yEd doesn't recognize 'name'
    .g <- .rmstknn(d,k,min.size,verbose)
    V(.g)$label <- V(.g)$name    
    
    return(.g)
}

# Internal function tracking level of recursion
# It asumes all checks on variables have been done on public interface
#
.rmstknn <- function(d,k=NULL,min.size=20,verbose=FALSE,level=0) {
    tini <- Sys.time()
    nc <- dim(d)[1]
    level <- level+1
    
    if(verbose) {
        message(sprintf('[%d] Start...',level))
    }
    # Initial pass
    g <- .mstknn(d,k,verbose)
    c <- clusters(g)
    if(verbose) {
        message(sprintf('[%d] Components: %d ',level,c$no))
        message('  [',sprintf(' %d',c$csize),' ]')
    }
    if(c$no > 1) {
        for(j in 1:c$no) {
            if(c$csize[j] < min.size) {
                if(verbose)
                    message(sprintf('[%d] Analysing Component %d, size %d (< %d)',
                                    level,j,c$csize[j],min.size))
                next
            }
            if(verbose)
                message(sprintf('[%d] Analysing Component %d, size %d',
                                level,j,c$csize[j]))
            s1 <- c$membership == j
            g1 <- .rmstknn(d[s1,s1],k,min.size,verbose,level)
            c1 <- clusters(g1)
            if(c1$no > 1) {
                message(sprintf("[%d] Split cluster %d into %d",level,j,c1$no))
                # Edges no longer included, as character matrix
                ee <- get.edgelist(graph.difference(induced.subgraph(g,s1),g1))
                # and then as edge ids on the larger graph... (all this to look for edges by name, puaj)
                ee <- get.edge.ids(g,as.vector(t(ee)))
                if(any(ee == 0))
                    warning("Edge id not found")
                # Remove them
                g <- delete.edges(g,ee)
                # DEBUG
                cqcq <- clusters(g)
                message('  [',sprintf(' %d',cqcq$csize),' ]')
            }
            else {
                message(sprintf("[%d] Cluster %d: no change",level,j))
            }
        }
    }
    tend <- Sys.time()
    if(verbose) {
        c <- clusters(g)
        message(sprintf('[%d] rMSTkNN: %d clusters, %fs',level,c$no,difftime(tend,tini,units="secs")))
        summary(g)
        message('  [',sprintf(' %d',c$csize),' ]')
    }
    return(g)
}

# title         .kNNadjacency
# description   Connectivity and adjacency matrix of kNN graph
# details       Compute the adjacency matrix and connectivity of the kNN graph
#               determined by the distance matrix d, starting at the k neighbour nbr.
#               iniK.  The default behavior will iterate down the iniK until the graph
#               becomes disconnected.  Iterate == FALSE means a single step ;-)
#
# param d       The distance matrix
# param iniK    The initial (maximum) value of neighbors
# param iterate Wether to iterate looking for the minimum k such that the kNN is connected
#
# return        a list with the following elements:
# section Value:
# \describe{
#  \item{\code{connected}}{A logical indicating if the graph is connected}
#  \item{\code{k}}{The final k value}
#  \item{\code{adjacency}}{A symmetric sparse Matrix with the kNN adjacency}
# }
#
# The computation is not particularly optimised
.kNNadjacency <- function(d, iniK, iterate=TRUE, verbose=FALSE) {
    nc = dim(d)[1]
    am <- Matrix(0,nrow=nc,ncol=nc,sparse=TRUE)
    colnames(am) <- colnames(d)
    rownames(am) <- colnames(d) # Or it will not be symmetric !

    om <- matrix(0,nrow=nc,ncol=iniK)   # matrix of indexes
    onu <- matrix(1:nc,nrow=nc,ncol=2)  # To index the am matrix
    onl <- onu

    # TODO: optimise
    aini <- Sys.time()
    ai <- aini
    for(j in 1:nc) {
        o <- order(d[j,],na.last=T)[1:(iniK+1)]    # First element should be == j
        if(o[1] != j) message('Another 0 distance: ',j,o[1])
        om[j,] <- o[2:(iniK+1)]
    }
    ae <- Sys.time()
    if(verbose) 
        message(sprintf('  NN: nodes=%d, initial k=%d, time=%fs',
                        nc,iniK,difftime(ae,ai,units="secs")))

    isc <- FALSE
    if(!iterate) {
        for(j in 1:nc) {
            am[om[j,1:iniK],j] <- 1
            am[j,om[j,1:iniK]] <- 1
        }
        am <- forceSymmetric(am,uplo='U')
        
        ai <- Sys.time()
        g1 <- graph.adjacency(adjmatrix=am,mode='undirected',weighted=T)
        isc <- is.connected(g1)
        ae <- Sys.time()
        # if(verbose) message('  Check k=',iniK,' ',isc,' ',difftime(ae,ai,,units="secs"))
        result <- list(connected=isc, k=iniK, adjacency=am)
    }
    else {
        thisK <- 0
        while(thisK < iniK && !isc) {
            thisK <- thisK + 1
            ai <- Sys.time()
            onu[,2] <- om[,thisK]
            onl[,1] <- om[,thisK]
            am[onu] <- 1
            am[onl] <- 1
            am <- forceSymmetric(am,uplo='U')
            ae <- Sys.time()
            # if(verbose) message('  Adj for k=',thisK,' ',difftime(ae,ai,units="secs"))
            
#             ai <- Sys.time()
#             for(j in 1:nc) {
#                 n <- om[j,thisK] 
#                 am[n,j] <- 1
#                 am[j,n] <- 1
#             }
#             am <- forceSymmetric(am,uplo='U')
#             ae <- Sys.time()
#             if(verbose) message('  Adj for k=',thisK,' ',difftime(ae,ai,units="secs"))
            
            ai <- Sys.time()
            g1 <- graph.adjacency(adjmatrix=am,mode='undirected',weighted=T)
            isc <- is.connected(g1)
            ae <- Sys.time()
            # if(verbose) message('  Check k=',thisK,' ',isc,' ',difftime(ae,ai,units="secs"))
        }
        result <- list(connected=isc, k=thisK, adjacency=am)
    }
    if(verbose) {
        ae <- Sys.time()
        message(sprintf('  NN: final k=%d, conn=%d, time=%fs',
                        result$k, result$connected, difftime(ae,aini,units="secs")))
    }
    return(result)
}
