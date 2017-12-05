###############################################################################
#   CENTRE FOR BIOINFORMATICS, BIOMARKER-DISCOVERY & INFORMATION-BASED MEDICINE
#   THE UNIVERSITY UNIVERSITY OF NEWCASTLE
#   University Drive, Callaghan, NSW, 2308, AUSTRALIA
#
#   Contains functions for input and output in formats used in CIBM
#
#   Created on: 2013/07/10
#   Last modified on: 2013/10/30
#'  @author Renato Vimieiro
#'  @author Carlos Riveros
#                          
#   License: MIT <http://opensource.org/licenses/MIT>
#
#   Copyright (c) 2013 Renato Vimieiro
#

#' @title Load NBI files to R
#' @name read.nbi
#' @description Loads NBI formatted file to R.
#' @seealso \code{\link{read.table}}
#' @param file string containing path to file
#' @export
#' @return \item{cibm.data}{containing the data from the file}
#' @example examples/read-nbi.R
read.nbi <- function(file=NULL){
    if(is.null(file)) stop("file cannot be NULL")
    lines <- readLines(con=file)        
    .parse.data(lines)    
}

#' @title Load NBI files to R
#' @name read.distance
#' @description Loads NBI formatted file containing distance matrix to R.
#' @seealso \code{\link{read.table}}
#' @param file string containing path to file
#' @export
#' @return \item{dist}{containing the distance matrix from the file}
#' @example examples/read-nbi.R
read.distance <- function(file=NULL){
    if(is.null(file)) stop("file cannot be NULL")
    lines <- readLines(con=file)      
    as.dist(.parse.distance(lines))
}

.breakLine <- function(.text,.sep) unlist(strsplit(.text,.sep))

.parse.data <- function(lines){
    sep <- "\\s+"    
    dim <- as.integer(.breakLine(lines[2],sep))
    
    raw.data <- matrix(.breakLine(lines[3:(3+dim[1]-1)],sep),nrow=dim[1],ncol=dim[2]+1,byrow=T)
        
    features <- raw.data[,1]
    samples <- .breakLine(gsub("^\\s+|\\s+$","",lines[length(lines)-3]),sep)
    labels <- factor(.breakLine(gsub("^\\s+|\\s+$","",lines[length(lines)-1]),sep))
    result <- new("cibm.data")
    result@data <- as.data.frame(apply(raw.data[,seq(2,dim[2]+1)],2,as.numeric))
    labels(result) <- labels
    
    names(result@data) <- make.names(samples)
    rownames(result@data) <- make.names(features)
    
    properties(result) <- data.frame(row.names=samples)
    
    result
}

.parse.distance <- function(lines){
    sep <- "\\s+"    
    dim <- as.integer(lines[2])
    #cat(dim,"\n")
    raw.data <- matrix(.breakLine(lines[3:(3+dim-1)],sep),nrow=dim,ncol=dim+1,byrow=T)
    #cat(dim(raw.data),"\n")
    #print(head(raw.data))
    features <- raw.data[,1]
    result <- as.data.frame(
        apply(raw.data[,seq(2,dim+1)],2,as.numeric)
    )
    #cat(dim(result),"\n")
    names(result) <- make.names(features)
    rownames(result) <- make.names(features)
    result
}


#' @title Write NBI files
#' @name write.nbi
#' @description Writes data matrices to NBI formatted files.
#' @seealso \code{\link{read.nbi}, \link{write.table}}
#' @param x dataset
#' @param file string containing path to file
#' @param featuresInRows logical value indicating if features are in rows (TRUE) or columns (FALSE)
#' @param indexClass Index of row/column representing class labels. 
#'          Either an integer or "last" for the last row/column. Default is "last".
#' @param gzip logical value indicating if output should be compressed in gzip format         
#' @export
#' @example examples/write-nbi.R
write.nbi <- function(x,file=NULL,featuresInRows=T,indexClass="last",gzip=T){
    
    # Checking arguments
    if(is.null(file)) stop("file cannot be NULL")
    if(is.null(x)) stop("Data cannot be NULL")
    #stopifnot(is.logical(featuresInRows),"featuresInRows must be either TRUE or FALSE")
    
    # Opening file
    if(gzip){
        if(length(grep("gz",file)) == 0) file <- paste0(file,".gz",sep='')
        con <- gzfile(description=file,open="w")
    } else{
        con <- file(description=file,open="w")
    }
    
    if(class(x) == "cibm.data") .write.cibm.data(x,con)
    else .write.data.frame(x,con,featuresInRows,indexClass)

    # Closing file
    close(con)
}

.write.cibm.data <- function(x,con,sep="\t"){
    
    labels <- labels(x)
    samples <- samples(x)
    features <- features(x)
    
    nfeat <- length(features)
    nsam <- length(samples)
        
    # Writing
    cat("<MicroarrayData>\n",file=con,sep="")
    cat(sprintf("%d%s%d\n",nfeat,sep,nsam),file=con,sep="")
    
    mat <- ""
    for(row in seq(1,nfeat)){
        mat <- paste0(mat,features[row],sep,paste(as.character(x[row,]),sep="",collapse=sep),"\n")
    }
    cat(mat,file=con,sep="")
    
    # Yeah, I know, is a mistake to say Sample*s*Names, but have to 
    # keep it for backwards compatibility
    cat("<SamplesNames>\n",file=con,sep="")
    cat(samples,file=con,sep=sep)
    cat("\n",file=con,sep="")
    
    cat("<SamplesClasses>\n",file=con,sep="")    
    cat(labels,file=con,sep=sep)
    cat("\n",file=con,sep="")
        
    cat("<EndOfFile>\n",file=con,sep="")    
}

.write.data.frame <- function(x,con,featuresInRows=T,indexClass="last",sep="\t"){
    # Preprocessing    
    if(!featuresInRows){
        x <- t(x)
    }
    
    nfeat <- nrow(x)
    nsam <- ncol(x)
    
    index <- ifelse(indexClass == "last",nfeat,indexClass)
    labels <- paste(as.character(x[index,]),sep="",collapse=sep)
    x <- x[-index,]
    nfeat <- nfeat -1
    
    
    samples <- names(x)
    features <- rownames(x)
    
    if(is.null(samples)) samples <- sprintf("S%d",1:nsam)
    if(is.null(features)) features <- sprintf("F%d",1:nfeat)
    
    # Writing
    cat("<MicroarrayData>\n",file=con,sep="")
    cat(sprintf("%d%s%d\n",nfeat,sep,nsam),file=con,sep="")
    
    mat <- ""
    for(row in seq(1,nfeat)){
        mat <- paste0(mat,features[row],sep,paste(as.character(x[row,]),sep="",collapse=sep),"\n")
    }
    cat(mat,file=con,sep="")
    
    cat("<SamplesNames>\n",file=con,sep="")
    cat(samples,file=con,sep=sep)
    cat("\n",file=con,sep="")
    
    cat("<SamplesClasses>\n",file=con,sep="")    
    cat(labels,file=con,sep="")
    cat("\n",file=con,sep="")
    
    
    cat("<EndOfFile>\n",file=con,sep="")
}

#' @title Write distance matrices in NBI format
#' @name write.distance
#' @seealso \code{\link{read.distance}, \link{write.nbi}}
#' @param dm A distance object or matrix
#' @param file string containing path to file
#' @param gzip logical value indicating if output should be compressed in gzip format         
#' @export
#' @examples
#' \dontrun{
#' x <- data.frame(S1=1:4,S2=2:5,S3=3:6,row.names=letters[1:4])
#' dm <- dist(x)
#' write.distance(dm,"test.distance.nbi",gzip=F) # by default gzip=T
#' }

write.distance <- function(dm,file=NULL,gzip=T){
    
    # Checking arguments
    if(is.null(file)) stop("file cannot be NULL")
    if(is.null(dm)) stop("Data cannot be NULL")
    #stopifnot(is.logical(featuresInRows),"featuresInRows must be either TRUE or FALSE")
    
    # Opening file
    if(gzip){
        if(length(grep("gz",file)) == 0) file <- paste0(file,".gz",sep='')
        con <- gzfile(description=file,open="w")
    } else{
        con <- file(description=file,open="w")
    }
    
    dm <- as.matrix(dm)
    sep <- "\t"
    nfeat <- nrow(dm)
    features <- rownames(dm)
    
    # Writing
    cat("<DistanceData>\n",file=con,sep="")
    cat(sprintf("%d\n",nfeat),file=con,sep="")
    
    mat <- ""
    for(row in seq(1,nfeat)){
        mat <- paste0(mat,features[row],sep,paste(as.character(dm[row,]),sep="",collapse=sep),"\n")
    }
    cat(mat,file=con,sep="")

    # Some applications rely on this being written at the bottom.  Is the same as feature names, but...
    cat("<SamplesNames>\n",file=con,sep="")
    cat(features,file=con,sep=sep)
    cat("\n",file=con,sep="")
    
    cat("<EndOfFile>\n",file=con,sep="")
    
    # Closing file
    close(con)
}

#' @title Load Alfa-Beta-k files to R
#' @name read.abk
#' @description Loads Alfa-Beta-k formatted file to R.
#' @seealso \code{\link{read.table} \link{write.abk}}
#' @param file string containing path to file
#' @export
#' @return \item{cibm.data}{containing the data from the file}
#' @example examples/read-abk.R
read.abk <- function(file=NULL) {
    if(is.null(file)) stop("file cannot be NULL")
    lines <- readLines(con=file)        
    # Initial checks
    fLyt <- match(tolower(lines[1]), c("featuresinrows", "featuresincolumns"), nomatch=0)
    if(fLyt == 0) stop(paste("Unrecognised token at 1:",lines[1]))
    tPresent <- match(tolower(lines[2]), c("targetpresent", "targetnotpresent"), nomatch=0)
    if(tPresent == 0) stop(paste("Unrecognised token at 2:",lines[2]))
    tPos <- match(tolower(lines[3]), c("first", "last", "none", "no"), nomatch=0)
    if(tPos == 0) stop(paste("Unrecognised token at 3:",lines[3]))
    if(tPresent == 1 && tPos == 3) 
        stop(paste("Inconsistent target and position at 2, 3:", lines[2], lines[3]))
    
    # Number of features and samples (NOT rows and columns!) in data
    nFa <- as.integer(lines[4])
    nSa <- as.integer(lines[5])

    if(fLyt == 1)
        res <- .parse.abk.byRow(lines,nFa,nSa,(tPresent == 1),tPos)
    else
        res <- .parse.abk.byCol(lines,nFa,nSa,(tPresent == 1),tPos)

    result <- res[[1]]
    cL <- res[[2]]
    dd <- dim(result@data)
    if(dd[1] != nFa || dd[2] != nSa) stop("Data matrix block has inconsistent dimension")
    
    .parse.abk.optional(lines[cL:length(lines)],result)
}

.parse.abk.byRow <- function(lines,nFeat,nSamp,tgt,tpos){
    sep <- "\\s+"
    
    # column names.  First is dummy (discard only if one less than expected number of columns)
    cNames <- .breakLine(lines[6],sep)
    # cat(cNames,"\n")
    if(length(cNames)-1 == nSamp) cNames <- cNames[2:length(cNames)]
    if(length(cNames) != nSamp) stop("Wrong number of case names")
    # cat(cNames,"\n")
    
    cL <- 7     # Current line
    if(tgt) {
        # target given as first line after names
        if(tpos == 1) {
            labels <- factor(.breakLine(gsub("^\\s+|\\s+$","",lines[cL]),sep))
            cL <- cL + 1
        }
    }
    else {
        labels <- factor(rep(1,nSamp))
    }
    
    nDCa <- length(.breakLine(lines[cL],sep))    # # data columns
    raw.data <- matrix(.breakLine(lines[cL:(cL+nFeat-1)],sep),nrow=nFeat,ncol=nDCa,byrow=T)
    cL <- cL + nFeat
    # ... or at last line of data block
    if(tgt && tpos == 2) {
        labels <- factor(.breakLine(gsub("^\\s+|\\s+$","",lines[cL]),sep))            
        cL <- cL + 1
    }
    rNames <- raw.data[,1]
    result <- new("cibm.abk")

    result@data <- as.data.frame(apply(raw.data[,seq(2,nSamp+1)],2,as.numeric))
    rownames(result@data) <- make.names(rNames)
    names(result@data) <- make.names(cNames)
    labels(result) <- labels
    
    list(result,cL)
}

.parse.abk.byCol <- function(lines,nFeat,nSamp,tgt,tpos){
    sep <- "\\s+"
    
    # column names.  First is dummy (discard only if one less than expected number of columns)
    cNames <- .breakLine(lines[6],sep)
    cPosit <- match("classes", tolower(cNames), nomatch=0)
    # #names == nFa+2 then both dummy and 'classes' present
    if(length(cNames)-2 == nFeat) {
        cNames <- cNames[2:length(cNames)]
        if(cPosit == 0) 
            stop("No 'classes' token at 6, but expected by number.")
        if(tgt && ((tpos == 1 && cPosit != 1) || (tpos == 2 && cPosit != length(cNames))))
            stop("'classes' token at wrong position at 6")
        if(tpos == 1)
            cNames <- cNames[2:length(cNames)]
        else
            cNames <- cNames[1:nFeat]
    }
    # #names == nFa+1 then either dummy and no 'classes', or 'classes' and no dummy
    else if(length(cNames)-1 == nFeat) {
        if(cPosit > 0 && tpos == 1)
            cNames <- cNames[2:length(cNames)]
        else if(cPosit > 0 && tpos == 2)
            cNames <- cNames[1:nFeat]
        else if(cPosit == 0)
            cNames <- cNames[2:length(cNames)]
    }
    # #names == nFa then no dummy nor 'classes' token
    else if(length(cNames) == nFa) {
        
    }
    else
        stop(paste("Incorrect number of tokens at 6:", length(cNames), lines[6]))

    cL <- 7     # Current line
    nDCa <- length(.breakLine(lines[cL],sep))    # # data columns
    raw.data <- matrix(.breakLine(lines[cL:(cL+nSamp-1)],sep),nrow=nSamp,ncol=nDCa,byrow=T)
    cL <- cL + nSamp
    rNames <- raw.data[,1]
    result <- new("cibm.abk")
    
    if(tgt && tpos == 1) {
        labels <- factor(raw.data[,2])
        result@data <- as.data.frame(t(apply(raw.data[,seq(3,nDCa)],2,as.numeric)))
    }
    else if(tgt && tpos == 2) {
        labels <- factor(raw.data[,nDCa])
        result@data <- as.data.frame(t(apply(raw.data[,seq(2,nDCa-1)],2,as.numeric)))
    }
    else {    # labels not present
        labels <- factor(rep(1,nDCa-1))
        result@data <- as.data.frame(t(apply(raw.data[,seq(2,nDCa)],2,as.numeric)))
    }
    names(result@data) <- make.names(rNames)
    rownames(result@data) <- make.names(cNames)
    labels(result) <- labels
    
    list(result,cL)
}

.parse.abk.optional <- function(lines,res) {
    sep <- "\\s+"
    cL <- 1
    dd <- dim(res@data)
    # Load default for featureweights and caseweights
    if("featureweights" %in% names(res@featureAttr)) 
        res@featureAttr["featureweights"] <- rep(1,dd[1])
    else
        res@featureAttr <- data.frame(featureweights=rep(1,dd[1]))
    if("caseweights" %in% names(res@caseAttr)) 
        res@caseAttr["caseweights"] <- rep(1,dd[2])
    else
        res@caseAttr <- data.frame(caseweights=rep(1,dd[2]))
    
    while(cL < length(lines)) {
        key <- match(tolower(lines[cL]),c("featureweights","caseweights","featurecolours",
                                          "casecolours","featuresin","featuresout","beta"),
                     nomatch=0)
        cL <- cL + 1
        if(key == 1) {      # featureweights
            fw <- sapply(.breakLine(lines[cL],sep),as.numeric)
            if(length(fw) != dd[1]) warning("Wrong number of feature weights. Ignored")
            else res@featureAttr["featureweights"] <- fw
            cL <- cL + 1
        }
        else if(key == 2) { # caseweights
            cw <- sapply(.breakLine(lines[cL],sep),as.numeric)
            if(length(cw) != dd[2]) warning("Wrong number of case weights. Ignored")
            else res@caseAttr["caseweights"] <- cw
            cL <- cL + 1
        }
        else if(key == 3) { # featurecolours
            # should probably be saved as numbers
            fw <- .breakLine(lines[cL],sep)
            if(length(fw) != dd[1]) warning("Wrong number of feature colours. Ignored")
            else res@featureAttr["featurecolours"] <- fw
            cL <- cL + 1
        }
        else if(key == 4) { # casecolours
            # should probably be saved as numbers
            cw <- .breakLine(lines[cL],sep)
            if(length(cw) != dd[2]) warning("Wrong number of case colours. Ignored")
            else res@caseAttr["casecolours"] <- cw
            cL <- cL + 1
        }
        else if(key == 5) { # featuresin
            fw <- make.names(.breakLine(lines[cL],sep))
            ddi <- rownames(res@data) %in% fw
            res@featureAttr["featuresin"] <- ddi
            cL <- cL + 1
        }
        else if(key == 6) { # featuresout
            fw <- make.names(.breakLine(lines[cL],sep))
            ddi <- rownames(res@data) %in% fw
            res@featureAttr["featuresout"] <- ddi
            cL <- cL + 1
        }
        else if(key == 7) { # beta
            fw <- factor(.breakLine(lines[cL],sep))
            res.beta <- fw
            cL <- cL + 1
        }
        else
            warning(paste("Unrecognised optional token at", cL-1, lines[cL-1]))
    }
    res
}

#' @title Write discrete matrix in Alfa-Beta-k format
#' @name write.abk
#' @seealso \code{\link{read.abk}, \link{write.nbi}}
#' @param x An object of type \code{cibm.abk}
#' @param file string containing path to file
#' @param featuresInRows To specify major file format (ignored, only TRUE supported)
#' @param classes To specify where do class labels get written. One of 'first', 'last', 'none'.
#' If none, then no classes will be written even if present.
#' @param out.equalweights Normally, if all weights are equal they are not written to file.
#' @param gzip logical value indicating if output should be compressed in gzip format         
#' @export
# FIXME 131030: Example does not work
#' @examples
#' \dontrun{
#' x <- data.frame(S1=1:4,S2=2:5,S3=3:6,row.names=letters[1:4])
#' write.abk(x,"test.abk",gzip=F) # by default gzip=T
#' }
write.abk <- function(x,file=NULL,featuresInRows=T,classes="last",out.equalweights=F,gzip=T) {
    
    # Checking arguments
    if(is.null(file)) stop("file cannot be NULL")
    if(is.null(x)) stop("Data cannot be NULL")
    if(!inherits(x,"cibm.abk")) stop("Data is not a 'cibm.abk' object")
    # FIXME:  For the time being, only features in rows
    #stopifnot(is.logical(featuresInRows),"featuresInRows must be either TRUE or FALSE")

    dd <- dim(x@data)
    tPos <- match(classes,c("first","last","none"),nomatch=3)
    if(out.equalweights) {
        # Set default weights
        if(is.null(x@caseAttr["caseweights"])) x@caseAttr["caseweights"] <- rep(1,dd[2])
        if(is.null(x@featureAttr["featureweights"])) x@featureAttr["featureweights"] <- rep(1,dd[1])
    }
    # Do we write weights ?
    ocW <- (out.equalweights || !isTRUE(all.equal(x@caseAttr["caseweights"][1], x@caseAttr["caseweights"])))
    ofW <- (out.equalweights || !isTRUE(all.equal(x@featureAttr["featureweights"][1], x@featureAttr["featureweights"])))

    # Opening file
    if(gzip){
        if(length(grep("gz",file)) == 0) file <- paste0(file,".gz",sep='')
        con <- gzfile(description=file,open="w")
    } else{
        con <- file(description=file,open="w")
    }
    
    writeLines(c(
        "FEATURESINROWS",
        switch(tPos,"TARGETPRESENT","TARGETPRESENT","TARGETNOTPRESENT"),
        switch(tPos,"FIRST","LAST","NONE"),
        dd[1],
        dd[2]), con=con)
    cat(c("dummy", samples(x)),sep="\t", file=con)
    writeLines("", con=con)
    if(tPos == 1) {
        cat("", as.character(labels(x)), sep="\t", file=con)
        writeLines("", con=con)
    }
    write.table(x@data, file=con, quote=F, row.names=T, col.names=F, sep="\t")
    if(tPos == 2) {
        cat("", as.character(labels(x)), sep="\t", file=con)
        writeLines("", con=con)
    }
    
    if(ocW) {
        writeLines("CASEWEIGHTS", con=con)
        cat(unlist(x@caseAttr["caseweights"]), sep="\t", file=con)
        writeLines("", con=con)
    }
    if(ofW) {
        writeLines("FEATUREWEIGHTS", con=con)
        cat(unlist(x@featureAttr["featureweights"]), sep="\t", file=con)
        writeLines("", con=con)
    }
    
    for(k in names(x@caseAttr)) {
        p <- match(tolower(k), c("caseweights", "casecolours", "casecolors"), nomatch=0)
        if(p == 1)  next    # Already dealt with
        else if(p == 0) {
            warning(paste("Unknown case attribute", k, ". Ignored", sep=" "))
            next
        }
        writeLines(toupper(k), con=con)
        cat(unlist(x@caseAttr[k]), sep="\t", file=con)
        writeLines("", con=con)
    }
    for(k in names(x@featureAttr)) {
        p <- match(tolower(k), c("featureweights", "featurecolours", "featurecolors"), nomatch=0)
        if(p == 1)  next    # Already dealt with
        else if(p == 0) {
            warning(paste("Unknown feature attribute", k, ". Ignored", sep=" "))
            next
        }
        writeLines(toupper(k), con=con)
        cat(unlist(x@featureAttr[k]), sep="\t", file=con)
        writeLines("", con=con)
    }
    # FIXME:  Crappy solution
    if(.hasSlot(x,"beta")) {
        writeLines("BETA", con=con)
        cat(unlist(x@beta), sep="\t", file=con)
        writeLines("", con=con)
    }
    
    close(con)
}
