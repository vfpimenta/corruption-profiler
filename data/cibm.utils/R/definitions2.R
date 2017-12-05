#########################################################################################
#   CENTRE FOR BIOINFORMATICS, BIOMARKER-DISCOVERY & INFORMATION-BASED MEDICINE
#   THE UNIVERSITY UNIVERSITY OF NEWCASTLE
#   University Drive, Callaghan, NSW, 2308, AUSTRALIA
#
#   Contains utility functions frequently used in CIBM
#
#   Created on: 2013/08/14
#   Last modified on: 2013/08/14
#'  @author Carlos Riveros
#                          
#   License: MIT <http://opensource.org/licenses/MIT>
#
#   Copyright (c) 2013 Carlos Riveros
#

#' @title CIBM's Alfa-Beta-k data representation
#' @description This class contains the data representation used by \link{read.abk}
#' This class inherits from the virtual class \code{cibm.base} class (see \link{cibm.data-class}).
#' @section Slots:
#'  \describe{
#'     \item{\code{data}:}{A \code{data.frame} containing the numeric data. 
#'     Rows are features and columns samples}
#'     \item{\code{labels}:}{\code{factor}. Contains the labels of samples.}
#'     \item{\code{caseAttr}:}{\code{data.frame}. Contains additional (optional) information of samples.
#'     'weights' and 'colours' are defined as 1 by default}
#'     \item{\code{featureAttr}:}{\code{data.frame}. Contains additional (optional) information of features.
#'     'weights' and 'colours' are defined as 1 by default}
#'     \item{\code{beta}:}{vector of beta values used as target}
#'     
#'     \item{\code{data}:}{(inherited) A \code{data.frame} containing the numeric data. 
#'     Rows are features and columns samples}
#'     \item{\code{labels}:}{(inherited) \code{factor}. Contains the labels of samples.}
#'  }
#' @exportClass cibm.abk
#' @rdname cibm.abk-class
#' @seealso \link{read.abk} \link{write.abk} \link{cibm.base}
#' @aliases cibm.abk-class
setClass(Class="cibm.abk",representation(caseAttr="data.frame",featureAttr="data.frame",beta="numeric"),
         contains="cibm.base")

# setValidity("cibm.abk",
#     function(object){
#         msg <- NULL
#         if(!is.factor(object@labels)) msg <- "labels must be 'factor'"
# #         sdata <- ncol(object@data)
# #         slabels <- length(object@labels)
# #         sattributes <- nrow(object@properties)
# #         if(sdata != slabels || sdata != sattributes || slabels != sattributes) {
# #             msg <- sprintf("Number of samples must be the same.\nFound %d samples in 'data', 
# #                            %d in 'labels', and %d in 'properties'.",sdata,slabels,sattributes)
# #         }
#         if(is.null(msg)) TRUE
#         else msg                
#     }    
# )

#' @exportMethod format
setMethod("format",
          signature(x = "cibm.abk"),
          function (x) 
          {  
              if(length(x@caseAttr)>0){
                  y <- t(x@caseAttr)
                  dz <- dim(x@featureAttr)
                  dy <- dim(y)
                  # Prepare for rbind
                  colnames(y) <- samples(x)
                  # Create NA block.  One extra row to account for 'class'
                  zmm <- matrix(nrow=dy[1]+1,ncol=dz[2])
                  # Prepare for rbind
                  colnames(zmm) <- colnames(x@featureAttr)
                  # Put all together
                  cbind(rbind(callNextMethod(),format(y)),
                        rbind(format(x@featureAttr),format(zmm)))
              } else{
                  cat("An empty object of class",class(x),"\n")
              }    
          }
)


#' @title Accessor functions
#' 
#' @rdname cibm.abk-methods
#' @export
setGeneric("caseAttr",function(object){standardGeneric("caseAttr")},package="cibm.utils")

#' @rdname cibm.abk-methods
#' @export
setMethod("caseAttr",
          signature(object="cibm.abk"),
          function(object)
          {
              slot(object,"caseAttr")              
          }
)

#' @rdname cibm.abk-methods
#' @export
setGeneric("caseAttr<-",function(object,value){standardGeneric("caseAttr<-")},package="cibm.utils")

#' @name caseAttr<-
#' @rdname cibm.abk-methods
#' @export
setReplaceMethod("caseAttr",
                 signature(object="cibm.abk"),
                 function(object,value)
                 {
                     slot(object,"caseAttr") <- value
                     object
                 }
)

#' @rdname cibm.abk-methods
#' @export
setGeneric("featureAttr",function(object){standardGeneric("featureAttr")},package="cibm.utils")

#' @rdname cibm.abk-methods
#' @export
setMethod("featureAttr",
          signature(object="cibm.abk"),
          function(object)
          {
              slot(object,"featureAttr")              
          }
)

#' @rdname cibm.abk-methods
#' @export
setGeneric("featureAttr<-",function(object,value){standardGeneric("featureAttr<-")},package="cibm.utils")

#' @name featureAttr<-
#' @rdname cibm.abk-methods
#' @export
setReplaceMethod("featureAttr",
                 signature(object="cibm.abk"),
                 function(object,value)
                 {
                     slot(object,"featureAttr") <- value
                     object
                 }
)

setAs("cibm.abk","data.frame",function(from){
    cbind(t(from@data),from@caseAttr,from@labels)
})
