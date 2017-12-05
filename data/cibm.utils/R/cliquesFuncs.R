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


# TODO: Functions to compute cliques on an undirected graph
# 
# @Author: carlos
###############################################################################

#require(igraph)

# Function computing the list of cliques for a graph.
# Input:
#	graph		undirected input graph or forest
#	min.size	Minimum size of cliques being returned
# Returns:
#	A list of tuples (gr, csz, clqs):
#	  gr	The graph's connected component
#	  csz	A vector of sorted clique sizes
#	  clqs	A vector of cliques graphs, same length as csz.
#
#' @import igraph
all.maxCliques <- function(graph, min.size=4, verbose=T) {
    #require(igraph)
   #FIXME{Replace logging by cat as Carlos did at first} require(logging)

    # defines logger
    #FIXME .logger = getLogger('clusterFuncs.all.maxCliques')
    #if(verbose && !("writeToConsole" %in% names(.logger[['handlers']]))) addHandler(writeToConsole)
    
    #FIXME if(verbose) loginfo("Starting",logger=.logger)
    
    g1s <- decompose.graph(graph, min.vertices=min.size)
    
    #FIXME if(verbose) loginfo("Graph contains %d connected subgraphs\n",length(g1s),logger=.logger)
    
    cl1 <- list()
    if(length(g1s)>0) {
        for (j in 1:length(g1s)) {
            n <- vcount(g1s[[j]])
            e <- ecount(g1s[[j]])
#FIXME
#             if(verbose){
#                 loginfo("Subgraph[[%d]]: #e: %d, #v: %d\n",j,e,n,logger=.logger)
#                 loginfo("\tComponent vertices: %s\n",V(g1s[[j]]),logger=.logger)
#                 loginfo("\t%s\n",V(g1s[[j]])$name,logger=.logger)
#             }    
#END            
            u <- cliques(g1s[[j]],min=min.size)
            if (length(u) > 0) {
                
                #FIXME if(verbose) loginfo(">> Total cliques: %d\n", length(u), logger=.logger)
                
                csz <- vector('numeric',length(u))
                for (k in 1:length(u))
                    csz[k] <- length(u[[k]])
                oo <- order(csz)
                cl1 <- c(cl1, list(gr=g1s[[j]], csz=csz[oo], clqs=u[oo]))
                ss <- unique(csz)
#FIXME                
#                 if(verbose){
#                     for (k in 1:length(ss))
#                         loginfo("\t%d cliques of size %d\n",length(csz[csz==ss[k]]), ss[k],logger=.logger)
#                 }
#END                
            }
        }
    }
    #FIXME if(verbose) loginfo("Finished",logger=.logger)
    cl1
}

# Function to compute the (weighted) vertex centralities of a graph
# Input:
#	graph		The undirected graph 
#	use.weights	TRUE if the centrality is to be computed using the
#			eldge's 'weight' attribute
# Returns:
#	A vector of length(V(graph)) with the vertex centralities:
#	Count of edges to 1-neighbors, (unweighted case)
#	Sum of 1-neighbors edges weight (weighted case.names#
#' @import igraph
vertex.centrality <- function(graph, use.weights=FALSE) {
	#require(igraph)
	vv <- V(graph)
	cen <- vector(mode = 'numeric', length = length(vv))
	kk <- 1;
	for (k in vv) {
		if (use.weights)
			cen[kk] <- sum(E(graph)[k %--% neighbors(graph,k)]$weight)
		else #FIXME nei is defined in igraph for internal use in '[', may cause problems here
			cen[kk] <- length(V(graph)[nei(k)]) 
		kk <- kk + 1
	}
	cen
}

