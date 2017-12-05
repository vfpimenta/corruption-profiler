#########################################################################################
#   CENTRE FOR BIOINFORMATICS, BIOMARKER-DISCOVERY & INFORMATION-BASED MEDICINE
#   THE UNIVERSITY UNIVERSITY OF NEWCASTLE
#   University Drive, Callaghan, NSW, 2308, AUSTRALIA
#
#   Script for computing distance matrices based on Jensen-Shannon Divergen, CIBM's Robust metric,
#   or Pearson's, Spearman's, and Kendall's statistic. Not to be used as standalone.
#
#         
#   Created on: 2013/02/12
#   Author: Carlos Riveros
#   Modified by: Renato Vimieiro
#                          
#   License: MIT <http://opensource.org/licenses/MIT>
#
#   Copyright (c) 2013 Carlos Riveros, Renato Vimieiro
#
# 20130802: [CR] Fixed bug in non-JS distances.
# 20131118: [RV] Added cosine distance

#' @title CIBM distances
#' @name distance
#' @rdname cibm-distances
#' @author Carlos Riveros and Renato Vimieiro
#' @description Computes a distance matrix using one of the measures commonly used in CIBM.
#'              Currently, the measures available are the Jensen-Shannon distance,
#'              Spearman, Pearson, Kendall, the Robust, Cosine, and Cramer's V.
#' @details This function accepts \code{cibm.data}, \code{matrix} and \code{data.frame}.
#' Notice that \code{save.dist} will save the distance matrix as an \bold{RData} file.
#' @export
#' @param x a dataset to compute the distance
#' @param method the measure to be used. Possible values are: JS, cosine, robust, pearson, spearman, kendall, cosine
#'          cramersV
#' @param save.dist a logical value indicating if the distance matrix should be saved to a file
#' @param col.wise a logical value indicating if the distance should be computed between columns (TRUE) or rows (FALSE)
#' @param file the path of the file where to save the distance matrix as RData if \code{save.dist} is TRUE
#' @seealso \code{\link[stats]{dist} and \link[cluster]{daisy}}
#'          for categorical data and other measures
#' @return A 'dist' object containing the distance matrix
#' @examples
#' \dontrun{
#' data(alzheimer)
#' dm <- distance(alzheimer)
#' # Oops...should get an error
#' # By definition, Jensen-Shannon distance cannot be used with negative values
#' # This should work now
#' dm <- distance(alzheimer,"robust")
#' as.matrix(dm)[1:10,1:10]
#' }
distance <- function(x,method="JS",save.dist=FALSE,col.wise=TRUE,file=NULL){
    
    if(class(x) == 'cibm.data') x <- x@data
    nRow <- dim(x)[1]
    nCol <- dim(x)[2]
    
    #matches or produces error
    stopifnot(is.logical(col.wise))
    stopifnot(is.logical(save.dist))
    if (save.dist && is.null(file)) {
        stop("file can not be NULL")
    }
#    col.wise = match.arg(col.wise,c(TRUE,FALSE))
#    save.dist = match.arg(save.dist,c(TRUE,FALSE))
    theDistance = match.arg(method,c("JS","robust","pearson","spearman","kendall","cosine","cramersV"))
    
    if(!col.wise) myData = t(x)
    else myData = x
    
    # Jensen-Shannon distance computation    
    if(theDistance == "JS") {
        
        cat("Normalising to [0,1]\n")
        
        if(any(myData < 0.0)) stop("data can not be negative for JS distance")
        
        myNorm <- apply(myData, 2, function(x) sum(x,na.rm=T))
        mAux <- 1 / myNorm
        # The wrong way to do it
        # system.time(myNormData <- myData %*% diag(mAux))
        myNormData <- myData
        system.time(for(j in 1:nCol) myNormData[,j] <- myData[,j] * mAux[j])
        
        entroBit <- function(x) x * log(x)
        # system.time(lone <- apply(myNormData, 2, entroBit))
        system.time(loneEnt <- myNormData * log(myNormData))
        loneEnt[apply(loneEnt,2,is.nan)] <- 0.0
        
        loneEnt <- colSums(loneEnt,na.rm=T)
        
        distance <- matrix(0,ncol=nCol,nrow=nCol)
        # Factor the 0.5 out of the loop, myNormData is not used in other things
        myNormData <- 0.5 * myNormData
        
        pt <- proc.time()
        pt0 <- pt1 <- pt
        infoStep <- max(1L,nCol %/% 10)
        for(j in 1:(nCol-1)) {
            rr <- (myNormData[,(1+j):nCol] + myNormData[,1:(nCol-j)])
            #  rr <- 0.5 * (myNormData[,(1+j):nCol] + myNormData[,1:(nCol-j)])
            if (j < nCol-1) {
                mEnt <- colSums(rr * log(rr),na.rm=T)
            } else {
                mEnt[1] <- sum(rr * log(rr),na.rm=T)
            }

            for (k in 1:(nCol-j)) {
                JS <- -mEnt[k] + 0.5 * (loneEnt[j+k] + loneEnt[k])
                distance[j+k,k] <- distance[k,j+k] <- sqrt(JS)
            }
            if ((j %% infoStep) == 0) {
                myS <- j / infoStep
                ptp <- pt
                pt <- proc.time()
                cat("Step", myS, j, "Time:", pt - ptp, "\n")
            }
        }
        pt <- proc.time()
        cat("Jensen-Shannon distance. Time:", pt - pt0, "\n")
        colnames(distance) <- colnames(myData)
        rownames(distance) <- colnames(myData)
        
    } else if (theDistance == "pearson" || theDistance == "spearman" || theDistance == "kendall") {
        cat("Computing",theDistance,"correlation matrix\n")
        ccm <- cor(myData,method=theDistance)      # dim(cm) is nCol x nCol
        cat(dim(ccm),"\n")
        
        distance <- 1 - ccm
        
    } else if (theDistance == "robust") {
        cat("Computing Pearson correlation matrix\n")
        pcm <- cor(myData)		# dim(cm) is nCol x nCol
        cat(dim(pcm),"\n")
        cat("Computing Spearman correlation matrix\n")
        scm <- cor(myData,method="spearman")  	# dim(cm) is nCol x nCol
        cat(dim(scm),"\n")

        # Get rid of NA cells
        pcm[is.na(pcm)] <- 0.0
        scm[is.na(scm)] <- 0.0
        
        cat("Computing robust correlation matrix")
        rcm <- sqrt(pcm^2 + scm^2)
        # Computation of sign: positive if pearson and spearman agree on positive
        rcm[pcm < -scm] <- rcm[pcm < -scm] * (-1)
        
        # Distance 
        cat("Computing robust distance matrix\n")
        distance <- sqrt(2) - rcm
        
    } else if (theDistance == "cosine"){
        myData <- as.matrix(t(myData))
        myData[is.na(myData)] <- 0
        myData <- myData / sqrt(rowSums(myData^2)) #normalizing before
        distance <- 1 - myData%*%t(myData) #FIXME: inner product can be computed on GPUs 
                                           #       or in parallel if required
    } else if(theDistance == "cramersV"){
        myData <- as.matrix(myData)
        distance <- matrix(rep(0,nCol*nCol),nrow=nCol,dimnames=list(colnames(myData),colnames(myData)))
        system.time(
        for(i in 1:(nCol-1)){
            for(j in (i+1):nCol){
                ct <- table(myData[,i],myData[,j])
                nc <- ncol(ct)
                nr <- nrow(ct)
                suppressWarnings(chi2 <- chisq.test(ct)$statistic[[1]])
                distance[j,i] <- 1 - sqrt(chi2/(sum(ct)*min(nc-1,nr-1)))
            }
        }
        )
    } else {
        stop(sprintf("The distance %s is not supported in this version.\nPlease\
                     run help(\"distance\") to see possible choices.",theDistance))
    }
    
    distance <- as.dist(distance)
    
    if(save.dist)    # Here we have computed the ditance and write the RData file
        save(distance,file=file)
    #return distance
    distance
}


#' @title Probability matrix computation
#' @rdname prob-matrix-comp
#' @export
#' @author Renato Vimieiro and Carlos Riveros
#' @description Computes probabilities from data. 
#' @param values should be an 1-d array
#' @param na.zero a logical value to set if probability of NA should be set to zero or left as NA 
#' @param normalize a logical value to set if values should be scaled such that \eqn{\sum_i prob(i) = 1}
#' @return array replacing input values by their expected probability
pdfDiscrete <- function(values,na.zero=TRUE,normalize=TRUE){
    if(length(values)==0) return(c())
    probs = prop.table(table(values))    
    r = sapply(as.character(values),function(y) ifelse(is.na(y),ifelse(na.zero,0,NA),probs[[y]]),USE.NAMES=FALSE)    
    if(normalize) r = r/sum(r,na.rm=TRUE)
    r
}

#' @rdname prob-matrix-comp
#' @export
pdfContinuous <- function(values,na.zero=TRUE,normalize=TRUE){
    if(!is.numeric(values)) return(c())
    dens = density(values,na.rm=TRUE)
    probs = splinefun(dens$x,dens$y)
    r = sapply(as.character(values),function(y) ifelse(is.na(y),ifelse(na.zero,0,NA),probs(y)),USE.NAMES=FALSE)
    if(normalize) r = r/sum(r,na.rm=TRUE)
    r    
}
