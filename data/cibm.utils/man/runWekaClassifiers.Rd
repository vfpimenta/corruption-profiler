\name{runWekaClassifiers}
\alias{runWekaClassifiers}
\title{Run Weka classifiers on data}
\usage{
  runWekaClassifiers(.train, .test = NULL,
    classifiers = "classifiers", numFolds = 10,
    indexClass = ncol(.train),
    seed = as.integer(Sys.time()), parallel = FALSE)
}
\arguments{
  \item{.train}{Either a \code{data.frame} or a
  \code{\link{cibm.data}} object containing the data. If
  data.frame, then optional parameter \code{indexClass} may
  also be passed, indicating what column contains class
  labels (samples are in rows).}

  \item{.test}{Either a \code{data.frame} or a
  \code{\link{cibm.data}} object containing the test data.
  This argument is optional. If it is not passed, then
  statistics are obtained via a k-fold cross-validation
  process.}

  \item{classifiers}{character. An array of regular
  expressions describing what classifiers to use.  The
  default value \code{"classifiers"} matches all possible
  classifiers. The user may also specify classifiers by
  type. For instance, making
  \code{listClassifiers=c("trees","rules")} indicates that
  only tree and rule-based classifiers should be used. One
  can also filter classifiers by name as, for example,
  \code{listClassifiers=c("J48","SimpleCart")}. In this
  case, all classifiers that (partially) match those names
  will be selected. If the regular expression matches no
  classifier, then function stops with an error message.}

  \item{numFolds}{integer. Number of folds for
  cross-validation. Discarded if \code{.test} is
  available.}

  \item{indexClass}{integer. Column containing class
  labels. Only used if \code{.train} and \code{.test} are
  data.frames. Default value is the last column.}

  \item{seed}{\code{integer}. Random seed to be used in
  cross-validation. Default: current time.}

  \item{parallel}{\code{logical}. Should classifiers be
  trained in parallel? See details.}
}
\value{
  \item{performance}{One row per classifier. Columns
  contain statistics.} \item{predictions}{Predicted labels
  (factors). One row per sample. One column per
  classifier.}
}
\description{
  Obtains classification statistics by running Weka
  classifiers on datasets. If test set is not available,
  then results are based on k-fold cross-validation
  (default).
}
\details{
  The option parallel relies on a registered parallel
  backend to work (See \link[foreach-package]{foreach}).
  Currently, it does not work well with the
  \link[multicore-package]{multicore}, that means \bold{do
  not use the} \link[doMC]{doMC} package. The safest way to
  run this function in parallel so far is to use the
  \link[doSNOW-package]{doSNOW} package. If you do not have
  doSNOW installed, run the following code
  \code{install.packages("doSNOW",repo="http://cran.csiro.au")}.
  Once the package is installed, you may run the code

  \code{ cl <- makeCluster(rep("localhost",2),"SOCK") #
  creates two threads registerDoSNOW(cl) # register
  parallel backend # ... run your experiments
  stopCluster(cl) # to terminate the R processes started
  with makeCluster }

  For more details on how to set up the cluster see
  \link{http://www.stat.uiowa.edu/~luke/R/cluster/cluster.html}
  and the documentation of the package SNOW.
}
\examples{
data(iris)
# Running with cross-validation mode
runWekaClassifiers(iris,classifiers=c("trees","NaiveBayes$"))
# Running with train-test.set mode
# Dummy example. Using training as test set.
runWekaClassifiers(iris,iris,classifiers=c("J48"))

\dontrun{
# loading required libraries
library(cibm.utils)
library(doSNOW)

# creating the parallel backend
cl <- makeCluster(rep("localhost",2),"SOCK")
registerDoSNOW(cl)

# running both in parallel and sequentially
data(iris)
system.time(results.par <- runWekaClassifiers(iris,classifiers=c("trees","bayes"),seed=1,parallel=T))
system.time(results.seq <- runWekaClassifiers(iris,classifiers=c("trees","bayes"),seed=1,parallel=F))

# checking if the results are the same
identical(results.par,results.seq)

# stopping the other R processes
stopCluster(cl)
}
}
\author{
  Renato Vimieiro
}

