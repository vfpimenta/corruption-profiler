#########################################################################################
#   CENTRE FOR BIOINFORMATICS, BIOMARKER-DISCOVERY & INFORMATION-BASED MEDICINE
#   THE UNIVERSITY UNIVERSITY OF NEWCASTLE
#   University Drive, Callaghan, NSW, 2308, AUSTRALIA
#
#   Contains the function for computing the refinement of labels
#   for a data set
#         
#   Created on: 2014/02/27
#   Author: Renato Vimieiro
#                          
#   License: MIT <http://opensource.org/licenses/MIT>
#
#   Copyright (c) 2014 Renato Vimieiro
#

#' @title Refining labels
#' @name refine
#' @rdname refinement
#' @author Renato Vimieiro and Carlos Riveros
#' @references Landis JR, Koch GG (1977) The measurement of observer agreement for categorical data. 
#'              Biometrics 33: 159-174
#'              
#'              Fleiss JL (1971) Measuring nominal scale agreement among many raters. Psychol Bull 76: 378-382
#'              
#'              Fleiss JL, Levin B, Paik MC (2004) The Measurement of Interrater Agreement, 
#'              John Wiley & Sons, Inc. pp. 598-626.
#'              
#'              Hubert L, Arabie P (1985) Comparing partitions. Journal of Classification 2: 193-218.
#'              
#'              Vinh NX, Epps J, Bailey J (2009) Information Theoretic Measures for Clusterings Comparison: Is
#'                a Correction for Chance Necessary? In: Proceedings of the 26th Annual International Conference
#'                on Machine Learning. pp. 1073-1080.
#'
#'
#' @description Refines labels of a data set iteratively based on a set of classifiers.
#'              
#'
#' @details This function accepts \code{cibm.data}, \code{matrix} and \code{data.frame}.
#'          In the case of \code{matrix} and \code{data.frame}, features are assumed to be in columns and
#'          \bold{class labels} must be in the \bold{last column}.
#' 
#'              The label refinement is done iteratively based on the output of given classifiers trained 
#'              with the input data.
#'              At each stage, new labels are predicted on a cross-validation setting
#'              using the classifiers trained with the original input data and labels obtained
#'              in the previous iteration. The algorithm starts with the original labels then
#'              assign new labels until the stopping criterion is met.
#'              There are 3 stopping criteria:
#'              \itemize{
#'              \item \bold{All labels} are the same as in previous iteration;
#'              \item \bold{Fixed} number of iterations;
#'              \item The \bold{Adjusted Rand Index} between the previous and current labels is above a given threshold
#'              \item The Fleiss' \bold{kappa} statistic between the previous and current 
#'              labels is above a given threshold
#'                 (it can also be one of the levels defined by Landis and Koch)
#'              }
#'              
#'              No threshold value needs to be supplied with the first criterion. For the second criterion,
#'              the number of iterations (an integer) must be supplied.            
#'              In the case of the last two criteria, the threshold must be a value
#'              between 0 and 1. If the chosen criterion is \emph{kappa}, 
#'              then one of the options may be given as well:
#'              \tabular{lr}{
#'              \bold{Input} \tab \bold{Equivalente threshold} \cr
#'              \emph{poor} \tab 0 \cr
#'              \emph{slight} \tab 0.01 \cr
#'              \emph{fair} \tab 0.21 \cr
#'              \emph{moderate} \tab 0.41 \cr
#'              \emph{substantial} \tab 0.61 \cr
#'              \emph{perfect} \tab 0.81 \cr
#'              }
#'              
#'              Apart from refining labels, a list of features might also be refined throughout the
#'              process. In this case, the function supplied in \code{.FUN} is applied to the data set
#'              to filter the features that will be used to train the classifiers. If the supplied function
#'              makes use of the labels (i.e. it is a supervised feature selection), then the current labels will
#'              be used.
#'              
#'              The function supplied in \code{.FUN}, in this version (we hope to improve in the future), has
#'              to have a single parameter, which is the data set with \bold{features in columns} and the last
#'              column is assumed to be the class labels. It must return an array of type \code{logical} 
#'              indicating whether a feature was selected or not. The positions is this array should match
#'              the columns (same order) of the input data.
#' @export
#' @param .data a data set to refine labels
#' @param .indexClass the column corresponding to class labels (only used if .data is a \code{matrix} or \code{data.frame})
#' @param .classifiers a list of classifiers to be used for refining the labels
#' @param .method the stopping criteria ("all","fixed","ari","kappa")
#' @param .threshold a threshold to be used in the stopping criteria (see Details)
#' @param .FUN a function object to be used for feature selection. If NULL (default) all features are used.
#' @param numFolds the number of folds to be used in cross-validation
#' @param verbose displays messages showing the progress
#' @param seed \code{integer}. Random seed to be used by classifiers. Default: current time.
#' @param parallel \code{logical}. Used internally in \link[cibm.utils::runWekaClassifiers]{runWekaClassifiers}.
#' @param max.iter \code{integer} The maximum number of iterations the algorithm will run independently
#'                  of the chosen stopping criteria. If this value is smaller than the \emph{fixed} number of
#'                  iterations, then it will be adjusted accordingly and a warning message will be given.
#' @param max.iter.same.labels \code{integer} The maximum number of iterations with no label updates 
#'                              before stopping the execution. Overrides all other stopping criteria.
#' @param ... extra arguments to be passed to \code{.FUN}
#' @seealso \code{\link[irr]{kappam.fleiss}}, \code{\link[mclust]{adjustedRandIndex}} 
#'          and \code{\link[cibm.utils]{runWekaClassifiers}}
#' @return A \code{list} object containing the following fields:
#' \describe{
#' \item{data}{A \code{data.frame} containing with selected (or all if .FUN is NULL) features and refined labels after running the procedure.}
#' \item{labels}{An array with refined labels (same as the ones in data).}
#' \item{iterations}{Number of iterations to refine labels.}
#' \item{ari}{Average adjusted Rand index across all iterations.}
#' \item{kappa}{Average kappa statistic across all iterations.}
#' \item{cramersV}{Average Cramer's V statistics for all classifiers across all iterations.}
#' \item{call}{Function call that produced this result.}
#' }
#' @importFrom plyr ddply
#' @importFrom mclust adjustedRandIndex
#' @importFrom irr kappam.fleiss
#' @examples
#' data(iris)
#' x <- refine(iris,5,c("J48","NaiveBayes$"),"fixed",10,verbose=TRUE)
#' x$labels == iris$Species
#' str(x)

# TODO: Store label countings (how many classifiers assigned the label to the sample)
refine <-
    function(.data,.indexClass=NULL,.classifiers,.method=c("all","fixed","ari","kappa"),
             .threshold=NULL,.FUN=NULL,numFolds=10,verbose=FALSE, seed=as.integer(Sys.time()),
             parallel=F, max.iter=100, max.iter.same.labels=5, ...)
    {   
        ################################
        # Local variables
        ################################        
        refine.env <- environment()
        iteration <- 1
        iter.same.labels <- 0
        
        # The idea here is that the seed passed as argument will be used
        # to set the seed of the local random number generator 
        # (that's the reason for restoring the old seed on exit).
        # Then, random numbers between 0 and 10^7 will be used as 
        # seeds for the runWekaClassifiers function.
        oldSeed <- .Random.seed
        on.exit( { .Random.seed <<- oldSeed } )        
        set.seed(seed)
        
        # Stores the sum of ari and kappa values across the execution
        stats <- c(0,0,0)
        
        if(inherits(.data,"cibm.data")){
            previousLabels <- labels(.data)
            .data <- as.data.frame(t(.data@data))
            .data$class <- previousLabels
            .indexClass <- ncol(.data)
        } else if(class(.data) %in% c("data.frame","matrix")){
            stopifnot(!is.null(.indexClass))
            previousLabels <- .data[,.indexClass]            
        } else{
            stop("Data has to be a cibm.data object, a data.frame, or a matrix.")
        }
        
        if(!is.numeric(.threshold)){
            if(.method %in% c("fixed","ari")){
                stop("Non-numeric thresholds are only accepted with kappa method.")
            } else{
                kappaTable <- array(c(0,.01,.21,.41,.61,.81),
                                    dimnames=list(c("poor","slight","fair",
                                                    "moderate","substantial","perfect")))
                .threshold <- kappaTable[[.threshold]]
            }
        } else{
            if(.method == 'fixed' && max.iter < .threshold){
                warning("max.iter smaller than threshold. Replacing its value with .threshold")
                max.iter <- .threshold
            }
        }
                
        currentLabels <- previousLabels
        
        noClasses <- length(unique(previousLabels))
        
        originalLevels <- levels(currentLabels)
        
        # Will change after discarding more inconsistent samples
        validSamples <- !is.na(currentLabels)
        # Might change if feature selection is part of the process
        validFeatures <- rep(TRUE,ncol(.data))
        
        ################################
        # Defining stopping criteria
        ################################
        # The syntax is weird, but the intention with these functions
        # is that their internal variables are global variables in the refine function.
        # So, as those values are updated, they will be used correctly by the functions.
        # This is to simplify the syntax of the main loop below.
        fixed <- function(){iteration >= .threshold}        
        allLabels <- function(){identical(previousLabels,currentLabels)}
        ari <- function(){ ariStat > .threshold}
        kappa <- function(){ kappaStat > .threshold}
        tryCatch(
            stoppingCriterion <- get(grep(.method,ls(refine.env),value=T),
                                     mode="function",envir=refine.env),
            error = function(e) stop(sprintf("Invalid stopping criterion (%s).",.method))
        )

        ################################
        # Defining majority of labels
        ################################
        .majority <- function(x){ 
            
            originalLevels[
                which(table(factor(t(x),levels=originalLevels)) > 0.5 * ncol(resultClassifiers$predictions))[1]
                ]
        }

        ################################
        # Main loop
        ################################
        start <- proc.time()
        repeat{
            if(verbose){
                message(sprintf("Starting iteration %d",iteration))
                infoFeatures <- ""
                if(!is.null(.FUN)){
                    infoFeatures <- sprintf("\n    %d active features\n    %d inactive features",
                                            sum(validFeatures),sum(!validFeatures))
                }
                message(sprintf("Data set currently contains:%s\n    %d active samples\n    %d inactive samples",
                                infoFeatures,sum(validSamples),sum(!validSamples)))
            }
            
            
            if(!is.null(.FUN)){
                fs.start <- proc.time()
                if(verbose){
                    message("Running feature selection")
                }
                validFeatures <- .FUN(.data,.indexClass,list(...))
                fs.end <- proc.time()
                execTime <- fs.end - fs.start
                if(verbose){
                    message(sprintf("A total of %d new features were selected in %.3fs",
                                    sum(validFeatures,na.rm=T),execTime[["elapsed"]]))
                }
            }
            
            
            weka.start <- proc.time()
            if(verbose){
                message("Running Weka classifiers")
                message(sprintf("Seed %d",seed))
            }
            #  Applying classifiers
            resultClassifiers <- runWekaClassifiers(.train=.data[validSamples,validFeatures],
                                                    classifiers=.classifiers,
                                                    indexClass=.indexClass,seed=as.integer(runif(1)*10^7),
                                                    parallel=parallel)
            
            # Getting new labels
            previousLabels <- .data[validSamples,.indexClass]            
            labelCounts <- ddply(cbind(resultClassifiers$predictions,id=seq(nrow(resultClassifiers$predictions))),
                                 .(id),.fun=.majority)
            currentLabels <- factor(labelCounts[,2],levels=originalLevels) # first column is the ID (rownames)            
            .data[validSamples,.indexClass] <- currentLabels
            
            if(identical(previousLabels,currentLabels)){
                iter.same.labels <- iter.same.labels + 1
            } else{
                iter.same.labels <- 0
            }        
            
            if(verbose){
                rownumbers <- which(validSamples)
                changedLabels <- rownumbers[which(currentLabels != previousLabels)]
                inconsistent <- rownumbers[which(is.na(currentLabels))]
                message(sprintf("Total label alterations: %d\nChanged labels: [%s]\nInconsistent: [%s]",
                                length(changedLabels)+length(inconsistent),
                                paste(changedLabels,collapse=" "),
                                paste(inconsistent,collapse=" ") ))
            }
            # Discarding inconsistent samples
            validSamples <- !is.na(.data[,.indexClass])
            
            weka.end <- proc.time()
            execTime <- weka.end - weka.start
            
            # Checking if there still exist non-inconsistent samples
            if(!any(validSamples)){
                stop("Infeasible solution. All samples were discarded.")
            } 
            # FIXME [RV] 20140302: Display labels discarded in the current iteration            
            classCount <- table(currentLabels)
            if(any(classCount==0) || length(classCount) < noClasses){
                warning("Dropping empty class label.")
            }
            
            # Computing quality statistics
            # Conversions to factor(...) are required because
            # adjustedRandIndex and kappam.fleiss remove NAs before
            # computing statistics. In case labels change to inconsistent
            # from one iteration to another (i.e. become NA), then NA is assigned
            # a special label so it is kept during the computations.
            ariStat <- adjustedRandIndex(factor(previousLabels,exclude=NULL,
                                                levels=c(levels(previousLabels),NA),
                                                labels=c(levels(previousLabels),"NA")),
                                         factor(currentLabels,exclude=NULL,
                                                levels=c(levels(currentLabels),NA),
                                                labels=c(levels(currentLabels),"NA")))
# 
#             kappaStat <- kappam.fleiss(cbind(factor(previousLabels,exclude=NULL,
#                                                     levels=c(levels(previousLabels),NA),
#                                                     labels=c(levels(previousLabels),"NA")),
#                                              factor(currentLabels,exclude=NULL,
#                                                     levels=c(levels(currentLabels),NA),
#                                                     labels=c(levels(currentLabels),"NA"))))$value
            
            # Experimenting with kappa calculated over all predictions rather
            # than on previous and current labels. Tests with METABRIC data showed that
            # kappa is constantly low (strangely ARI was always high).
            # Also, since we are seeking consensus of classifiers about samples' labels,
            # it makes more sense to check if they agree or not.
            # This also suggests that ARI shouldn't be used as a stopping criterion,
            # because it can only be computed for pairs of labellings.
            kappaStat <- kappam.fleiss(resultClassifiers$predictions)$value        

            stats <- stats + c(ariStat,kappaStat,mean(resultClassifiers$performance[,"cramerV"],na.rm=T))
            if(verbose){
                message(sprintf("New labels obtained in %.3fs",execTime[["elapsed"]]))
                message(sprintf("Performance (Cramer's V):\n    Average\t%.3f\n    Stdev\t%.3f\n    Max\t%.3f\n    Min\t%.3f",
                                mean(resultClassifiers$performance[,"cramerV"],na.rm=T),
                                sd(resultClassifiers$performance[,"cramerV"],na.rm=T),
                                max(resultClassifiers$performance[,"cramerV"],na.rm=T),
                                min(resultClassifiers$performance[,"cramerV"],na.rm=T)))
                message(sprintf("Agreement:\n    kappa\t%.3f\n    ARI\t%.3f",kappaStat,ariStat))
            }
            # Stops if the criterion is met
            if(stoppingCriterion() || iteration >= max.iter || iter.same.labels >= max.iter.same.labels ){
                break
            }
            iteration <- iteration + 1
        }
        end <- proc.time()
        execTime <- end - start
        if(verbose){            
            message(sprintf("Task finished in %.3fs",execTime[["elapsed"]]))
        }
        
        list(data=.data[,unique(which(validFeatures)),drop=F],
             labels=.data[,.indexClass],
            iterations=iteration,
            ari=stats[[1]]/iteration,
            kappa=stats[[2]]/iteration,
            cramersV=stats[[3]]/iteration,
            iter.same.labels=iter.same.labels,
            call=match.call())
    }