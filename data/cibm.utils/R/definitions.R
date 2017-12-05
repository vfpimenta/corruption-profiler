#########################################################################################
#   CENTRE FOR BIOINFORMATICS, BIOMARKER-DISCOVERY & INFORMATION-BASED MEDICINE
#   THE UNIVERSITY UNIVERSITY OF NEWCASTLE
#   University Drive, Callaghan, NSW, 2308, AUSTRALIA
#
#   Contains utility functions frequently used in CIBM
#
#   Created on: 2013/08/06
#   Last modified on: 2013/08/06
#'  @author Renato Vimieiro
#                          
#   License: MIT <http://opensource.org/licenses/MIT>
#
#   Copyright (c) 2013 Renato Vimieiro
#
#   20130815: [CR] Changed the definition of classes such that cibm.base is the virtual class
#   for both cibm.data (with added properties) and cibm.abk (which requires more specialisation)

#' @title CIBM's data representation
#' @description Virtual base class for data representation used by \link{cibm.data} and \link{cibm.abk}
#' @section Slots:
#'  \describe{
#'     \item{\code{data}}{A \code{data.frame} containing the numeric data. 
#'     Rows are features and columns samples}
#'     \item{\code{labels}:}{\code{factor}. Contains the labels of samples.}
#'  }
#' @exportClass cibm.base
#' @rdname cibm.data-class
#' @seealso \link{cibm.data} \link{cibm.abk}
#' @aliases cibm.base-class
setClass(Class="cibm.base",representation(data="data.frame",labels="factor","VIRTUAL"))

setValidity("cibm.base",
    function(object){
        msg <- NULL
        if(!is.factor(object@labels)) msg <- "Labels must be 'factor'"
#         sdata <- ncol(object@data)
#         slabels <- length(object@labels)
#         sattributes <- nrow(object@properties)
#         if(sdata != slabels || sdata != sattributes || slabels != sattributes) {
#             msg <- sprintf("Number of samples must be the same.\nFound %d samples in 'data', 
#                            %d in 'labels', and %d in 'properties'.",sdata,slabels,sattributes)
#         }
        if(is.null(msg)) TRUE
        else msg                
    }    
)

#' @exportMethod format
setMethod("format",
    signature(x = "cibm.base"),
    function (x) 
    {  
        if(length(x@data)>0){
            # cat("Aqui 1\n")
            y=t(data.frame(class=format(x@labels)))
            colnames(y) <- colnames(x@data)
            rbind(format(x@data),y)
        } else{
            cat("An empty object of class",class(x),"\n")
        }    
    }
)

#' @exportMethod print
setMethod("print",
    signature(x = "cibm.base"),
    function (x, ...) 
    {
        print(format(x))
    }
)

#' @exportMethod show
setMethod("show",
    signature(object = "cibm.base"),
    function (object) 
    {
        print(object)
    }
)

#' @exportMethod [
setMethod("[",
    signature(x = "cibm.base"),
    function (x, i, j, ..., drop = TRUE) 
    {
        x@data[i,j, ..., drop=drop]
    }
)

#' @exportMethod [<-
setMethod("[<-",
    signature(x = "cibm.base"),
    function (x, i, j, ..., value) 
    {
        x@data[i,j, ...] <- value
    }
)

#' @exportMethod [[
setMethod("[[",
    signature(x = "cibm.base"),
    function (x, i, j, ...) 
    {
        x@data[[i,j, ...]]
    }
)

#' @exportMethod [[<-
setMethod("[[<-",
    signature(x = "cibm.base"),
    function (x, i, j, ..., value) 
    {
        x@data[[i,j, ...]] <- value
    }
)

#' @title Accessor functions
#' 
#' @param object \code{cibm.base}. An object to retrieve set information.
#' @rdname cibm.data-methods
#' @export
#' @aliases labels,cibm.data,cibm.data-methods
#' @docType methods
setGeneric("labels",function(object){standardGeneric("labels")},package="cibm.utils")

#' @rdname cibm.data-methods
#' @exportMethod labels
setMethod("labels",
    signature(object="cibm.base"),
    function(object)
    {
        slot(object,"labels")
    }
)

#' @param value Data to set \code{labels}
#' @rdname cibm.data-methods
#' @export
setGeneric("labels<-",function(object,value){standardGeneric("labels<-")},package="cibm.utils")

#' @name labels<-
#' @rdname cibm.data-methods
#' @exportMethod labels<-
setReplaceMethod("labels",
          signature(object="cibm.base"),
          function(object,value)
          {
              slot(object,"labels") <- as.factor(value)
              object
          }
)

#' @param object Of class \code{cibm.base}. An object to retrieve set information.
#' @rdname cibm.data-methods
#' @export
#' @aliases samples,cibm.data,cibm.data-methods
#' @docType methods
setGeneric("samples",function(object){standardGeneric("samples")},package="cibm.utils")

#' @rdname cibm.data-methods
#' @exportMethod samples
setMethod("samples",
          signature(object="cibm.base"),
          function(object)
          {
              names(object@data)
          }
)

#' @param object Of class \code{cibm.base}. An object to retrieve set information.
#' @rdname cibm.data-methods
#' @export
#' @aliases features,cibm.data,cibm.data-methods
#' @docType methods
setGeneric("features",function(object){standardGeneric("features")},package="cibm.utils")

#' @rdname cibm.data-methods
#' @exportMethod features
setMethod("features",
          signature(object="cibm.base"),
          function(object)
          {
              rownames(object@data)
          }
)

#' @title CIBM's data representation
#' @description This class contains the data representation used by \link{read.nbi}
#' @section Slots:
#'  \describe{
#'     \item{\code{data}}{A \code{data.frame} containing the numeric data. 
#'     Rows are features and columns samples}
#'     \item{\code{labels}:}{\code{factor}. Contains the labels of samples.}
#'     \item{\code{properties}:}{\code{data.frame}. Contains additional (optional) information of samples.}
#'     
#'     \item{\code{data}:}{(inherited) A \code{data.frame} containing the numeric data. 
#'     Rows are features and columns samples}
#'     \item{\code{labels}:}{(inherited) \code{factor}. Contains the labels of samples.}
#'  }
#' @exportClass cibm.data
#' @rdname cibm.data-class
#' @seealso \link{read.nbi} \link{cibm.base}
#' @aliases cibm.data-class
setClass(Class="cibm.data",representation(properties="data.frame"),contains="cibm.base")

#' @rdname cibm.data-methods
#' @export
setGeneric("properties",function(object){standardGeneric("properties")},package="cibm.utils")

#' @rdname cibm.data-methods
#' @export
setMethod("properties",
          signature(object="cibm.data"),
          function(object)
          {
              slot(object,"properties")              
          }
)

#' @rdname cibm.data-methods
#' @export
setGeneric("properties<-",function(object,value){standardGeneric("properties<-")},package="cibm.utils")

#' @name properties<-
#' @rdname cibm.data-methods
#' @export
setReplaceMethod("properties",
                 signature(object="cibm.data"),
                 function(object,value)
                 {
                     slot(object,"properties") <- value
                     object
                 }
)

setAs("cibm.data","data.frame",function(from){
    cbind(t(from@data),from@properties,from@labels)
})
