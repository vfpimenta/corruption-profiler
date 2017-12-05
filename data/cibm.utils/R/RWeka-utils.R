###############################################################################
#   CENTRE FOR BIOINFORMATICS, BIOMARKER-DISCOVERY & INFORMATION-BASED MEDICINE
#   THE UNIVERSITY UNIVERSITY OF NEWCASTLE
#   University Drive, Callaghan, NSW, 2308, AUSTRALIA
#
#   Contains utilitary functions from RWeka to copy data into Weka
#   These functions were retrieved from RWeka/R/readers.R
#

read_data_into_Weka <-
    function(x, classIndex = 0L)
    {
        ## FastVector was deprecated in Weka >= 3-7-1. Now we have to use
        ## the List interface (see the cast of ArrayList in the Attribute
        ## constructor).
        
        ## See the Weka 3-5-7 source code for this insanity (e.g., string).
        ## Note that the class index, if any, must be set as an attribute.
        
        ## As Weka instance objects do not have case/row names, we store
        ## such information in the R container for the Weka instances.  For
        ## simplicity, we store the dimnames including the (variable) names
        ## also contained in the Weka instances.
        dx <- dim(x)
        dnx <- dimnames(x)
        
        ## Build attribute information
        attname <- names(x)
        attinfo <- .jnew("java/util/ArrayList", 
                         as.integer(length(x)))
        for (i in seq_along(x)) {
            ## Make logicals into Weka nominals.
            if(is.logical(x[[i]]))
                x[[i]] <- factor(x[[i]])
            attribute <- 
                if(is.factor(x[[i]])) {
                    levels <- .jnew("java/util/ArrayList", 
                                    as.integer(nlevels(x[[i]])))
                    sapply(levels(x[[i]]), function(k)
                        .jcall(levels, "Z", "add", 
                               .jcast(.jnew("java/lang/String", k),
                                      "java/lang/Object")))
                    ## shift to Weka's internal coding
                    x[[i]] <- as.double(x[[i]]) - 1
                    .jnew("weka/core/Attribute", attname[i], 
                          .jcast(levels, "java/util/List"))
                }
            else if(is.character(x[[i]])) {
                att <- .jnew("weka/core/Attribute", attname[i],
                             .jnull("java/util/List"))
                x[[i]] <- as.factor(x[[i]])
                index <- sapply(levels(x[[i]]), function(k)
                    .jcall(att, "I", "addStringValue", k))
                if(any(index < 0))
                    stop("pushing to Type 'string' failed")
                x[[i]] <- as.double(index[as.integer(x[[i]])])
                
                att
            }
            else if(inherits(x[[i]], "Date")) {
                att <- .jnew("weka/core/Attribute", attname[i],
                             "yyyy-MM-dd")
                x[[i]] <- .jcall("RWekaInterfaces", "[D", "parseDate", att,
                                 .jarray(format(x[[i]])),
                                 NA_character_)
                att
            }
            else if(inherits(x[[i]], "POSIXt")) {
                att <- .jnew("weka/core/Attribute", attname[i],
                             "yyyy-MM-dd HH:mm:ss")
                ## Normalize to local time.
                x[[i]] <- .jcall("RWekaInterfaces", "[D", "parseDate", att,
                                 .jarray(format(x[[i]], tz = "")),
                                 NA_character_)
                att
            }
            else if(is.numeric(x[[i]]))
                .jnew("weka/core/Attribute", attname[i])
            else
                stop("Type not implemented")
            .jcall(attinfo, "Z", "add",
                   .jcast(attribute, "java/lang/Object"))
        }
        
        ## Build instances.
        n <- dim(x)[1L]                     # number of instances
        instances <- .jnew("weka/core/Instances",
                           "R_data_frame",  # FIXME
                           attinfo,
                           as.integer(n))   # capacity
        
        ## Set class index.
        if(classIndex > 0L)
            .jcall(instances, "V", "setClassIndex",
                   as.integer(classIndex - 1L))
        
        ## Populate.
        x <- unlist(x, use.names = FALSE)
        x[is.na(x)] <- NaN                  # Weka missing value.
        .jcall("RWekaInterfaces", "V", "addInstances",
               instances, .jarray(x), as.integer(n))
        
        ## Note that using dim and dimnames attributes would result in a
        ## matrix, which seems a bad idea.
        .structure(instances, .dim = dx, .dimnames = dnx)
    }

read_model_frame_into_Weka <-
    function(mf)
    {
        ## Model frame has the class variable in first position.
        read_data_into_Weka(mf, 1L)
    }


.structure <-
    function(x, ...)
        `attributes<-`(x, c(attributes(x), list(...)))