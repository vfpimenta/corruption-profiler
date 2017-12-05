###############################################################################
#   CENTRE FOR BIOINFORMATICS, BIOMARKER-DISCOVERY & INFORMATION-BASED MEDICINE
#   THE UNIVERSITY UNIVERSITY OF NEWCASTLE
#   University Drive, Callaghan, NSW, 2308, AUSTRALIA
#
#   Contains functions for obtaining the performance of Weka classifiers on
#   datasets.
#
#   Created on: 2013/10/30
#   Last modified on: 2013/11/05
#'  @author Renato Vimieiro
#                          
#   License: MIT <http://opensource.org/licenses/MIT>
#
#   Copyright (c) 2013 Renato Vimieiro
#

#' @title Run Weka classifiers on data
#' @name runWekaClassifiers
#' @description Obtains classification statistics by running Weka classifiers on
#'              datasets. If test set is not available, then results are based on
#'              k-fold cross-validation (default).
#' @param .train Either a \code{data.frame} or a \code{\link{cibm.data}} object
#'              containing the data. If data.frame, then optional parameter \code{indexClass}
#'              may also be passed, indicating what column contains class labels (samples are in rows).
#' @param .test Either a \code{data.frame} or a \code{\link{cibm.data}} object containing the test data.
#'              This argument is optional. If it is not passed, then statistics are obtained via a k-fold
#'              cross-validation process.
#' @param classifiers character. An array of regular expressions describing what classifiers to use.
#'                  The default value \code{"classifiers"} matches all possible classifiers. The user may
#'                  also specify classifiers by type. For instance, making 
#'                  \code{listClassifiers=c("trees","rules")} indicates that only tree and rule-based
#'                  classifiers should be used. One can also filter classifiers by name as, for example,
#'                  \code{listClassifiers=c("J48","SimpleCart")}. In this case, all classifiers that
#'                  (partially) match those names will be selected. If the regular expression matches no 
#'                  classifier, then function stops with an error message.
#' @param numFolds integer. Number of folds for cross-validation. Discarded if \code{.test} is available.
#' @param indexClass integer. Column containing class labels. Only used if \code{.train} and \code{.test}
#'          are data.frames. Default value is the last column.
#' @param seed \code{integer}. Random seed to be used in cross-validation. Default: current time.
#' @param parallel \code{logical}. Should classifiers be trained in parallel? See details.
#' @details The option parallel relies on a registered parallel backend to work 
#'          (See \link[foreach-package]{foreach}). Currently, it does not work well with
#'          the \link[multicore-package]{multicore}, that means \bold{do not use the} \link[doMC]{doMC}
#'          package. The safest way to run this function in parallel so far is to use the
#'          \link[doSNOW-package]{doSNOW} package. If you do not have doSNOW installed, run the following
#'          code \code{install.packages("doSNOW",repo="http://cran.csiro.au")}. Once the package is installed,
#'          you may run the code 
#'          
#'          \code{
#'          cl <- makeCluster(rep("localhost",2),"SOCK") # creates two threads
#'          registerDoSNOW(cl) # register parallel backend
#'          # ... run your experiments
#'          stopCluster(cl) # to terminate the R processes started with makeCluster
#'          }
#'          
#'          For more details on how to set up the cluster see 
#'          \link{http://www.stat.uiowa.edu/~luke/R/cluster/cluster.html} and
#'          the documentation of the package SNOW.
#' @export
#' @return \item{performance}{One row per classifier. Columns contain statistics.}
#'          \item{predictions}{Predicted labels (factors). One row per sample. One column per
#'          classifier.}
#' @import RWeka
#' @importFrom plyr ldply ddply
#' @examples
#' data(iris)
#' # Running with cross-validation mode
#' runWekaClassifiers(iris,classifiers=c("trees","NaiveBayes$"))
#' # Running with train-test.set mode
#' # Dummy example. Using training as test set.
#' runWekaClassifiers(iris,iris,classifiers=c("J48"))
#' 
#' \dontrun{
#' # loading required libraries
#' library(cibm.utils)
#' library(doSNOW)
#' 
#' # creating the parallel backend
#' cl <- makeCluster(rep("localhost",2),"SOCK")
#' registerDoSNOW(cl)
#' 
#' # running both in parallel and sequentially
#' data(iris)
#' system.time(results.par <- runWekaClassifiers(iris,classifiers=c("trees","bayes"),seed=1,parallel=T))
#' system.time(results.seq <- runWekaClassifiers(iris,classifiers=c("trees","bayes"),seed=1,parallel=F))
#' 
#' # checking if the results are the same
#' identical(results.par,results.seq)
#' 
#' # stopping the other R processes
#' stopCluster(cl)
#' }
#' 
runWekaClassifiers <- function(.train,.test=NULL,classifiers="classifiers",numFolds=10,
                               indexClass=ncol(.train),seed=as.integer(Sys.time()),parallel=FALSE){
    if(length( (listClassifiers <- getClassifiers(classifiers)) ) < 1 ) 
        stop("None of classifiers were found.")
    if(inherits(.train,"cibm.data")) { 
        .train <- cbind(as.data.frame(t(.train@data)),class=labels(.train))
        indexClass <- ncol(.train)
    }    
    crossval <- is.null(.test)
    if(!crossval && inherits(.test,"cibm.data")) {
        .test <- cbind(as.data.frame(t(.test@data)),class=labels(.test))
        # Check if they have same features
        stopifnot(any(names(.test) != names(.train)))
    }
    form <- as.formula(paste(names(.train)[indexClass],"~ ."))
    computeMCC <- length(unique(.train[,indexClass])) == 2
    statClass <- function(classifier){
#         logf <- file(paste("~/mcc-",classifier,".log",sep=""),open="wt")
#         sink(file=logf)
#         on.exit({sink();close(logf)})
#         print(ls())
        tryCatch({
        message(classifier)
        # If execution is in parallel, then all variables from the parent environment (do.ply)
        # have to be copied to the global environments of each worker, otherwise they will
        # not find the variables.
        if(parallel){            
            .GlobalEnv <- parent.env(environment())
#             print(ls(.GlobalEnv))
        }
        desc <- unlist(strsplit(gsub("weka[/\\.]classifiers[\\./]","",classifier),"[/\\.]"))[2:1] 
        myClass.func <- make_Weka_classifier(classifier)                
        myClass.model <- tryCatch(myClass.func(form,.train),
                                  error = function(e) { warning(e); NULL} )        
        #myClass.model <- myClass.func(form,.train)        
        if(is.null(myClass.model)) { 
            warning(sprintf("Discarded: %s\n",classifier))
            predictions[,desc[1]] <- NULL #discard column
            return(NULL) 
        }
        #if(crossval) {
        # FIXME [RV:20140813]
        # We can call the Weka's Evaluation class directly
        # rather than using RWeka's evaluate_Weka_classifier.
        # Internally they are doing exactly what we do in
        # predict_XVal. This avoids invoking cross-validation
        # twice for getting the predictions and statistics.
        if(is.null(.test)){
            stats <- evaluate_Weka_classifier(myClass.model,numFolds=numFolds,seed=seed)            
        } else{    
            stats <- evaluate_Weka_classifier(myClass.model,newdata=.test)
        }    
        X2 <- chisq.test(stats$confusionMatrix)$statistic[[1]]
        ct <- stats$confusionMatrix + 0.0000001 #avoiding NaNs
        
        if(!computeMCC){
            stats <- data.frame(classifier=desc[1],type=desc[2],
                       t(stats$details),
                       cramerV=sqrt(X2/(sum(ct)*(nrow(ct)-1))))
        } else{            
            stats <- data.frame(classifier=desc[1],type=desc[2],
                       stats$details,
                       cramerV=sqrt(X2/(sum(ct)*(nrow(ct)-1))),
                       mcc=(ct[1,1]*ct[2,2]-ct[2,1]*ct[1,2])/sqrt((ct[1,1]+ct[2,1])*(ct[1,1]+ct[1,2])*
                                                    (ct[2,2]+ct[2,1])*(ct[2,2]+ct[1,2]))
                       )            
        }
        if(crossval) {            
            tmp <- predict_XVal(myClass.model,numFolds,seed)
            #print(head(tmp))
            #predictions[row.names(tmp),desc[1]] <<- tmp$predicted
            predictions <- tmp[,"predicted",drop=F]
            names(predictions) <- desc[1]
        } else{
            predictions <- data.frame(row.names=rownames(.test),rep(NA,nrow(.test)))
            names(predictions) <- desc[1]
            predictions[,desc[1]] <- predict(myClass.model,newdata=.test)
        }        
        result <- list(performance=stats,predictions=predictions)
        #print(result)
        result
        #stats
        },error = function(e) { sprintf("Discarded: %s\n",classifier); NULL} )
    }
    
#     # Adding variable to environment so above function can add results to it
        if(crossval) predictions <- data.frame(row.names=rownames(.train))
        else    predictions <- data.frame(row.names=rownames(.test))

    # FIXME [RV:20140312] require that the user register the parallel backend
    # outside this function. Gives more flexibility, but it is less straightforward.
    # Major issue is that doMC DOES NOT WORK with RWeka. It gets stuck when classifier
    # is evaluated. SNOW works fine. So, this should be preferred.
    # If package doMC is available, then run tests in parallel
#     if(parallel){
#         if(!require(doMC)){
#             stop("To run this function in parallel, please install package \"doMC\".
# Run install.packages(\"doMC\",repo=\"http://cran.csiro.au\")")
#         }
#         registerDoMC(cores=parallel::detectCores(logical = TRUE))
#     }
        
    #exportVars <- c("crossval","form","numFolds","parallel",
    #                "predictions","seed",".test",".train")
    exportVars <- NULL
    # Performance will have the results of evaluating classifier
    result <- llply(listClassifiers,statClass,.parallel=parallel,
                         .paropts=list(.export=exportVars,
                                       .packages=c("cibm.utils","RWeka"),
                                       .errorhandling = "stop",
                                       .verbose=F),
                         .inform=T)    
    for(col in result) if(!is.null(col)) predictions <- cbind(predictions,col$predictions[rownames(predictions),,drop=F])
    list(performance=ldply(result,function(x) x$performance),predictions=predictions)
}

# This function returns the Weka classes for
# classifiers that match the regular expression given
# as an argument. Notice that the list contains
# 50 classifiers hard-coded on the function.
# To include new classifiers, add them to the array
# 'classifiers'.
#' @title List of available Weka classifiers
#' @name getClassifiers
#' @description Returns a list of Weka classes that match
#' the given regular expression or the full list if 
#' regex is not present.
#' @param regex A list of regular expressions. Default value is NULL, which makes
#' function return full list of available classifiers.
#' @return \code{character} An array of Weka classes matching \emph{regex}.
#' @export
#' @examples
#' # Full list
#' getClassifiers()
#' # All trees and bayes classifiers
#' getClassifiers(c("trees","bayes"))
#' # Getting a specific classifier
#' getClassifiers("J48$")
getClassifiers <- function(regex=NULL){
    classifiers <- c(
        #"weka.classifiers.bayes.AODE", #removed from weka 3.7
        #"weka.classifiers.bayes.AODEsr",
        #"weka.classifiers.bayes.BayesianLogisticRegression",
        #"weka.classifiers.bayes.HNB",
        "weka.classifiers.bayes.BayesNet",
        "weka.classifiers.bayes.NaiveBayes",
        "weka.classifiers.bayes.NaiveBayesUpdateable",
        #"weka.classifiers.bayes.WAODE",
        "weka.classifiers.functions.Logistic",
        #"weka.classifiers.functions.RBFNetwork",
        "weka.classifiers.functions.MultilayerPerceptron",
        "weka.classifiers.functions.SimpleLogistic",
        "weka.classifiers.functions.SMO",
        #"weka.classifiers.functions.SPegasos",
        "weka.classifiers.functions.VotedPerceptron",
        #"weka.classifiers.functions.Winnow",
        #"weka.classifiers.lazy.IB1",
        "weka.classifiers.lazy.IBk",
        "weka.classifiers.lazy.KStar",
        "weka.classifiers.lazy.LWL",
        "weka.classifiers.meta.AdaBoostM1",
        "weka.classifiers.meta.AttributeSelectedClassifier",
        "weka.classifiers.meta.Bagging",
        #"weka.classifiers.meta.ClassificationViaClustering",
        "weka.classifiers.meta.ClassificationViaRegression",
        "weka.classifiers.meta.CostSensitiveClassifier",
        "weka.classifiers.meta.CVParameterSelection",
        #"weka.classifiers.meta.Dagging",
        #"weka.classifiers.meta.Decorate",
        #"weka.classifiers.meta.END",
        "weka.classifiers.meta.LogitBoost",
        #"weka.classifiers.meta.MultiBoostAB",
        "weka.classifiers.meta.MultiClassClassifier",
        "weka.classifiers.meta.MultiScheme",
        "weka.classifiers.meta.RandomCommittee",
        "weka.classifiers.meta.RandomSubSpace",
        #"weka.classifiers.meta.RotationForest",
        "weka.classifiers.meta.Stacking",
        "weka.classifiers.meta.Vote",
        "weka.classifiers.rules.DecisionTable",
        "weka.classifiers.rules.JRip",
        #"weka.classifiers.rules.NNge",        
        "weka.classifiers.rules.OneR",
        "weka.classifiers.rules.PART",
        #"weka.classifiers.rules.Prism",
        #"weka.classifiers.rules.Ridor",
        "weka.classifiers.rules.ZeroR",
        #"weka.classifiers.trees.ADTree",
        #"weka.classifiers.trees.BFTree",
        "weka.classifiers.trees.DecisiomStump",
        #"weka.classifiers.trees.FT",
        "weka.classifiers.trees.HoeffdingTree",
        #"weka.classifiers.trees.Id3",
        "weka.classifiers.trees.J48",
        #"weka.classifiers.trees.LADTree",
        "weka.classifiers.trees.LMT",
        #"weka.classifiers.trees.NBTree",
        "weka.classifiers.trees.RandomForest",
        "weka.classifiers.trees.RandomTree",
        "weka.classifiers.trees.REPTree"
        #"weka.classifiers.trees.SimpleCart"        
       )
    if(is.null(regex)) classifiers
    else grep(paste(regex,collapse='|'),classifiers,ignore.case=T,value=T)
}

#' @title Predict class labels in a cross-validation setting
#' @name predict_XVal
#' @description Partition data in k-folds, then create model using
#'              k-1 folds and predict labels for samples in remaining
#'              folds using the model. Finally returns all labels.
#' @param classifier \code{\link[RWeka]{Weka_classifiers}}. The classifier to use in cross-validation.
# @param .formula \code{"\link[=formula.object]{formula}"} to be used by the classifier to build the model.
# @param .data \code{data.frame}
#' @param numFolds \code{integer}.
#' @param seed \code{integer}. Random seed to be used in cross-validation. Default: current time.
#' @export
#' @importFrom plyr ddply mutate
#' @import rJava
#' @return \code{data.frame} Containing: the 'actual' label; the 'predicted' label; 'error' indicator; and
#'            'prediction' reliability
# @return \code{data.frame} The input data containing two extra columns: one with the fold information
# for each sample; and the second with the predicted class label.
predict_XVal <- function(classifier,numFolds,seed=as.integer(Sys.time())){    
    outputPreds <- .jnew("weka/classifiers/evaluation/output/prediction/CSV")
    predictions <- .jnew("java/lang/StringBuffer")
    .jcall(outputPreds,"V","setBuffer",predictions)
    .jcall(outputPreds,"V","setAttributes",.jnew("java/lang/String","last"))
    mf <- model.frame(classifier)
    mf$ID <- row.names(mf)
    instances <- read_model_frame_into_Weka(mf)
    wrap <- classifier$classifier
    filter <- .jnew("weka/filters/unsupervised/attribute/RemoveType")
    evaluation <- .jnew("weka/classifiers/Evaluation", instances)
    classifier <- .jnew("weka.classifiers.meta.FilteredClassifier")        
    .jcall(classifier,"V","setFilter",.jcast(filter,"weka/filters/Filter"))
    .jcall(classifier,"V","setClassifier",.jcast(wrap,"weka/classifiers/Classifier"))
    random <- .jnew("java/util/Random",.jlong(seed))
    #random <- .jnew("java/util/Random")
    .jcall(evaluation,"V","crossValidateModel",
           .jcast(classifier,"weka/classifiers/Classifier"),
           instances, as.integer(numFolds), random, .jarray(outputPreds))
    
    x <- unlist(strsplit(.jcall(predictions,"S","toString"),"\n"))
    b <- ldply(x[-1],function(y) unlist(strsplit(y,",")))
    names(b) <- unlist(strsplit(x[1],","))
    rownames(b) <- b$ID
    repl <- function(x) gsub("\\d+:","",x)
    b$actual <- repl(b$actual)
    b$predicted <- repl(b$predicted)
    b[,2:5]
}
