#########################################################################################
#   CENTRE FOR BIOINFORMATICS, BIOMARKER-DISCOVERY & INFORMATION-BASED MEDICINE
#   THE UNIVERSITY UNIVERSITY OF NEWCASTLE
#   University Drive, Callaghan, NSW, 2308, AUSTRALIA
#
#   Contains utility functions frequently used in CIBM
#
#   Created on: 2013/06/19
#   Last modified on: 2013/10/30
#'  @author Renato Vimieiro
#                          
#   License: MIT <http://opensource.org/licenses/MIT>
#
#   Copyright (c) 2013 Renato Vimieiro
#

#' @name CM1
#' @title CM functions
#' @rdname CM-functions
# @seealso \code{\link{CM2}}
#' @export
#' @import parallel
#' @description Computes CM values of features
#' @details These functions compute the CM statistics for a dataset.
#' \eqn{CM_1(f,X,Y)=\frac{\mu^f_X - \mu^f_Y}{1 + max(y_f) - min(y_f)}}{
#' CM_1(f,X,Y)=(\mu^f_X - \mu^f_Y)/(1 + max(y_f) - min(y_f))} and 
#' \eqn{CM_2(f,X,Y)=\frac{\mu^f_X - \mu^f_Y}{1 + max(y_f) - min(y_f)}}{
#' CM_2(f,X,Y)=(\mu^f_X - \mu^f_Y)/(1 + min(max(y_f) - min(y_f),max(x_f) - min(x_f))}.
#' 
#' The parameter of this function can either be a \code{data.frame} or a \code{cibm.data}.
#' In the first case, \code{labels} must be specified. It must be a \code{factor}.
#' 
#' @param .data a \code{\link{data.frame}} or \code{\link{cibm.data}}; rows are features, columns samples
#' @param .labels \code{\link{factor}} containing the class labels of samples; 
#' @param .parallel \code{logical} Should CM1 of each class be computed in parallel?
#' @return \item{\code{\link{data.frame}}}{one column per label and one row per feature}
#' 
#' @examples
#' \dontrun{
#' data(alzheimer)
#' cm1_alzheimer <- CM1(alzheimer)
#' head(cm1_alzheimer)
#' }
CM1 <- function(.data,.labels=NULL,.parallel=FALSE){        
    if(class(.data) == "cibm.data") .labels <- labels(.data)            
    classes <- levels(.labels)  
    .cm1 <- function(label){    
        target = which(.labels == label)        
        avg_target = rowMeans(.data[,target])
        avg_others = rowMeans(.data[,-c(target)])
        mins = apply(.data[,-c(target)],1,min)
        maxs = apply(.data[,-c(target)],1,max)
        (avg_target - avg_others)/(1 + abs(maxs - mins))
    }    
    #FIXME: 131030 - parallel lapply not available for Windows at the momemnt. Check if it is still true
    if (.Platform$OS.type == "windows" || !.parallel) {
    	result <- base::lapply(classes,FUN= .cm1)
    } else{	
        result <- parallel::mclapply(classes,mc.cores=parallel::detectCores(), FUN= .cm1)
    }    
    #result <- mclapply(classes,mc.cores=detectCores(), FUN= .cm1)
    result <- as.data.frame(result)
    names(result) <- classes
    result
}

#' @name CM2
# @title CM2 function
#' @rdname CM-functions
# @seealso \code{\link{CM1}}
#' @export
#' @import parallel
# @description Computes CM2 values of features (rows)
# @param .data a \code{\link{data.frame}}; rows are features, columns samples
# @param .labels a \code{\link{data.frame}} containing the class labels of samples; 
# @return \item{\code{\link{data.frame}}}{one column per label and one row per feature}
CM2 <- function(.data,.labels){    
    if(class(.data) == "cibm.data") .labels <- labels(.data) 

    classes = levels(.labels)
    .cm2 <- function(label){
        target <- which(.labels == label)
        avg_target <- rowMeans(.data[,target])
        avg_others <- rowMeans(.data[,-c(target)])
        
        mins_target <- apply(.data[,target],1,min)
        maxs_target <- apply(.data[,target],1,max)
        rangeTarget <- abs(maxs_target - mins_target)
        
        mins_others <- apply(.data[,-c(target)],1,min)
        maxs_others <- apply(.data[,-c(target)],1,max)
        rangeOthers <- abs(maxs_others - mins_others)
        
        (avg_target - avg_others)/min((1 + rangeTarget),(1 + rangeOthers))
    }
    #FIXME: 131030 - parallel lapply not available for Windows at the momemnt. Check if it is still true
    result <- if (.Platform$OS.type == "windows") base::lapply(classes,FUN= .cm2)
                mclapply(classes,mc.cores=detectCores(),FUN=.cm2)
    result <- as.data.frame(result)
    names(result) <- classes
    result
}

# TODO: @author Renato Vimieiro
#       Function needs to be tested more carefully to assess real applicability
# RH1 <- function(.data,.labels){    
#     classes = levels(.labels$class)
#     result = list()
#     #samples = sapply(classes,function(x) rownames(.labels)[which(.labels$class==x)])
#     apply_fun = lapply
#     require(parallel)
#     if(nrow(.data) > 5000){
#         apply_fun = function(.d,.f) mclapply(.d,FUN=.f,mc.cores=detectCores())
#     }
#     
#     for(label in classes){        
#         CM1s = apply_fun(setdiff(classes,c(label)), function(label2) {
#             cat("Computing CM1",label,label2,"\n")
#             .classes = subset(.labels,class %in% c(label,label2))
#             .classes$class = droplevels(.classes$class)
#             x = CM1(.data[,rownames(.classes)],.classes)                                    
#             subset(x,select=1)
#         }
#         )
#         CM1s = as.data.frame(CM1s)
#         #names(CM1s) = setdiff(classes,c(label))
#         result[[label]] = apply(CM1s,1,median)
#     }
#     
#     as.data.frame(result)
# }
