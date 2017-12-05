#########################################################################################
#   CENTRE FOR BIOINFORMATICS, BIOMARKER-DISCOVERY & INFORMATION-BASED MEDICINE
#   THE UNIVERSITY UNIVERSITY OF NEWCASTLE
#   University Drive, Callaghan, NSW, 2308, AUSTRALIA
#
#   Created on: 2013/07/16
#   Author: Carlos Riveros
#   Modified by: Renato Vimieiro
#                          
#   License: MIT <http://opensource.org/licenses/MIT>
#
#   Copyright (c) 2013 Carlos Riveros
#

#' @name kNNCliques
#' @title Clustering based on kNN and Cliques
#' @author Carlos Riveros
#' @description Function to quickly analyse a distance matrix by searching cliques on
#'              progressively larger kNN islands. Results will be saved to file according 
#'              to \code{basename}.
#'              
#'              Graph filenames have the form 
#'                  \code{basename_}K\code{nn_}SS\code{_clique_<date>.<format>}
#'              where
#'                  K   is the number of neighbours in the kNN graph
#'                  SS  is the save keyword (top, top2, top3, all)
#'              
#'              K increases with each iteration.
#'              
#' @param dm Distance matrix, assumed possitive defined and symmetrical
#' @param MAX_ITER Maximum number of iterations to complete
#' @param basename Basename for the graph files written
#' @param format Graphic file format: gml, graphml other as supported by igraph
#' @param min.size  Min clique size.  If null, set to 2/3 of kNN
#' @param saveSz the size of maximum cliques to use: all, top, top2, or top3
#' @param verbose logical indicating if progress information should be displayed
#' @seealso \code{\link{kmeans}, \link[cluster]{pam}, \link{hclust}, \link{distance}}
#' @import igraph
#' @export
#' 
#' @examples
#' \dontrun{
#' # Load dataset
#' data(alzheimer)
#' #Compute distance matrix using the robust distance
#' dm <- distance(alzheimer,method="robust")
#' #Compute clusters
#' kNNCliques(dm,format="gml",saveSz="top2")
#' #Check if files were created
#' dir(pattern="iter.*")
#' }
kNNCliques <- function(dm, MAX_ITER=5, basename="iter", format="graphml",
                       min.size=NULL, saveSz=c("all","top2","top3","top"), verbose=T)
{
    
    if(class(dm) == "dist") dm <- as.matrix(dm)
    
    mmx <- max(dm)
    mmn <- min(dm[dm > 0.0])  # For non-zero entries
    
    if(verbose) cat(sprintf("Distance: mean:%f max:%f min:%f\n", mean(dm), mmx, mmn))
    
    nCol <- dim(dm)[1]
    
    # Compute the sorted indexes.  Trick to send self index to end
    mord <- apply(dm+diag(Inf,nCol),1,order)
    
    # Do the search for cliques, starting at k = 3 (cliques of at least size 4)
    knn <- 3
    go <- TRUE
    iter <- 1
    if (is.null(min.size))
        mincsz <- max((knn * 2) %/% 3, 3)
    else
        mincsz <- min.size
    
    while(go)
    {
        # Unitary Adjacency matrix
        uam <- matrix(data=0,nrow=nCol,ncol=nCol)
        for(k in 1:nCol) {
            uam[k,mord[1:knn,k]] <- 1
            uam[mord[1:knn,k],k] <- 1
        }
        colnames(uam) <- colnames(dm)
        
        # We construct a temporary graph for connectedness
        gr <- graph.adjacency(uam,mode="upper")
        gc <- is.connected(gr)
        if(verbose) {
            message(paste("Iteration:",iter,". Graph for kNN=",knn,gc))
            print(gr)
        }
        
        allcq <- all.maxCliques(gr,min.size=mincsz,verbose)
        
        cc_disjoint <- 0
        cc_sharing <- 0
        if (length(allcq) > 0) {
            
            for (ik in seq(to=length(allcq),from=1,by=3)) {
                
                thisgr <- allcq[[ik]]
                if(verbose) {
                    message(paste("  Component ",ik))
                    print(thisgr)  # The graph
                }
                thiscsz <- allcq[[ik+1]]
                oo <- order(thiscsz,decreasing=T)   # The list of clique sizes
                thisclqs <- allcq[[ik+2]]
                # Largest clique in collection
                thisclq <- induced.subgraph(thisgr, thisclqs[[oo[[1]]]])

                cc_disjoint <- cc_disjoint + 1      # Largest clique of component is always disjoint
                if(verbose) {
                   message(paste(oo[1],":",thisclqs[[oo[[1]]]],V(thisclq)$name,"*"))
                }
                
                lgstClq <- length(V(thisclq))
                # Collection graph, only for first component of iteration
                if (ik == 1)
                    collGr <- graph.edgelist(get.edgelist(thisclq),directed=F)  # Preserve node names if original has
                for (ij in 2:length(oo)) {
                    contained <- FALSE
                    sharing <- FALSE
                    for (il in 1:(ij-1)) {
                        inters <- thisclqs[[oo[ij]]] %in% thisclqs[[oo[il]]]
                        contained <- all(inters)
                        sharing <- sharing || any(inters)
                        if (contained) {
                            if(verbose) cat(" Clique ",oo[ij],"is subclique of",oo[il],"\n")
                            break
                        }
                    }
                    if (!contained) {
                        thisclq <- induced.subgraph(thisgr, thisclqs[[oo[[ij]]]])
                        doSave <- switch(saveSz[1],
                                         all=TRUE,
                                         top2=(lgstClq-length(V(thisclq)))<2,
                                         top3=(lgstClq-length(V(thisclq)))<3,
                                         top=(lgstClq-length(V(thisclq)))<1)
                        if (doSave) {
                            collGr <- simplify(graph.edgelist(rbind(get.edgelist(collGr),get.edgelist(thisclq)),directed=F))
                            if(!sharing)
                                cc_disjoint <- cc_disjoint + 1
                            else
                                cc_sharing <- cc_sharing + 1
                        }
                       if(verbose){
                           if (!sharing)                            
                               message(paste(oo[ij],":",thisclqs[[oo[ij]]],V(thisclq)$name,"*"))
                           else
                               message(paste(oo[ij],":",thisclqs[[oo[ij]]],V(thisclq)$name))
                       }
                    }
                }
            }
            if (length(V(collGr)) > 0) {
                V(collGr)$label <- V(collGr)$name
                if(verbose) cat("Writing graph to file [%s]",sprintf("%s%d_%dnn_c%d_clique_%s.%s",basename,iter,knn,ik,as.Date(Sys.time()),format))
                write.graph(collGr,file=sprintf("%s_%dnn_%s_clique_%s.%s",basename,knn,saveSz[1],as.Date(Sys.time()),format),format=format)
            }
            if(verbose) {
                noc <- no.clusters(collGr)
                message(paste("Iteration:",iter,":",noc,"clusters",cc_disjoint,"independent cliques (",cc_sharing,"non-trivial)\n"))
            }
        }
        
        iter=iter+1
        knn <- knn + 1
        go <- iter < MAX_ITER
        if (is.null(min.size))
            mincsz <- max((knn * 2) %/% 3, 3)
    }
    
}
