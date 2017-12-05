\name{refine}
\alias{refine}
\title{Refining labels}
\usage{
  refine(.data, .indexClass = NULL, .classifiers,
    .method = c("all", "fixed", "ari", "kappa"),
    .threshold = NULL, .FUN = NULL, numFolds = 10,
    verbose = FALSE, seed = as.integer(Sys.time()),
    parallel = F, max.iter = 100, max.iter.same.labels = 5,
    ...)
}
\arguments{
  \item{.data}{a data set to refine labels}

  \item{.indexClass}{the column corresponding to class
  labels (only used if .data is a \code{matrix} or
  \code{data.frame})}

  \item{.classifiers}{a list of classifiers to be used for
  refining the labels}

  \item{.method}{the stopping criteria
  ("all","fixed","ari","kappa")}

  \item{.threshold}{a threshold to be used in the stopping
  criteria (see Details)}

  \item{.FUN}{a function object to be used for feature
  selection. If NULL (default) all features are used.}

  \item{numFolds}{the number of folds to be used in
  cross-validation}

  \item{verbose}{displays messages showing the progress}

  \item{seed}{\code{integer}. Random seed to be used by
  classifiers. Default: current time.}

  \item{parallel}{\code{logical}. Used internally in
  \link[cibm.utils::runWekaClassifiers]{runWekaClassifiers}.}

  \item{max.iter}{\code{integer} The maximum number of
  iterations the algorithm will run independently of the
  chosen stopping criteria. If this value is smaller than
  the \emph{fixed} number of iterations, then it will be
  adjusted accordingly and a warning message will be
  given.}

  \item{max.iter.same.labels}{\code{integer} The maximum
  number of iterations with no label updates before
  stopping the execution. Overrides all other stopping
  criteria.}

  \item{...}{extra arguments to be passed to \code{.FUN}}
}
\value{
  A \code{list} object containing the following fields:
  \describe{ \item{data}{A \code{data.frame} containing
  with selected (or all if .FUN is NULL) features and
  refined labels after running the procedure.}
  \item{labels}{An array with refined labels (same as the
  ones in data).} \item{iterations}{Number of iterations to
  refine labels.} \item{ari}{Average adjusted Rand index
  across all iterations.} \item{kappa}{Average kappa
  statistic across all iterations.} \item{cramersV}{Average
  Cramer's V statistics for all classifiers across all
  iterations.} \item{call}{Function call that produced this
  result.} }
}
\description{
  Refines labels of a data set iteratively based on a set
  of classifiers.
}
\details{
  This function accepts \code{cibm.data}, \code{matrix} and
  \code{data.frame}.  In the case of \code{matrix} and
  \code{data.frame}, features are assumed to be in columns
  and \bold{class labels} must be in the \bold{last
  column}.

  The label refinement is done iteratively based on the
  output of given classifiers trained with the input data.
  At each stage, new labels are predicted on a
  cross-validation setting using the classifiers trained
  with the original input data and labels obtained in the
  previous iteration. The algorithm starts with the
  original labels then assign new labels until the stopping
  criterion is met.  There are 3 stopping criteria:
  \itemize{ \item \bold{All labels} are the same as in
  previous iteration; \item \bold{Fixed} number of
  iterations; \item The \bold{Adjusted Rand Index} between
  the previous and current labels is above a given
  threshold \item The Fleiss' \bold{kappa} statistic
  between the previous and current labels is above a given
  threshold (it can also be one of the levels defined by
  Landis and Koch) }

  No threshold value needs to be supplied with the first
  criterion. For the second criterion, the number of
  iterations (an integer) must be supplied.  In the case of
  the last two criteria, the threshold must be a value
  between 0 and 1. If the chosen criterion is \emph{kappa},
  then one of the options may be given as well:
  \tabular{lr}{ \bold{Input} \tab \bold{Equivalente
  threshold} \cr \emph{poor} \tab 0 \cr \emph{slight} \tab
  0.01 \cr \emph{fair} \tab 0.21 \cr \emph{moderate} \tab
  0.41 \cr \emph{substantial} \tab 0.61 \cr \emph{perfect}
  \tab 0.81 \cr }

  Apart from refining labels, a list of features might also
  be refined throughout the process. In this case, the
  function supplied in \code{.FUN} is applied to the data
  set to filter the features that will be used to train the
  classifiers. If the supplied function makes use of the
  labels (i.e. it is a supervised feature selection), then
  the current labels will be used.

  The function supplied in \code{.FUN}, in this version (we
  hope to improve in the future), has to have a single
  parameter, which is the data set with \bold{features in
  columns} and the last column is assumed to be the class
  labels. It must return an array of type \code{logical}
  indicating whether a feature was selected or not. The
  positions is this array should match the columns (same
  order) of the input data.
}
\examples{
data(iris)
x <- refine(iris,5,c("J48","NaiveBayes$"),"fixed",10,verbose=TRUE)
x$labels == iris$Species
str(x)
}
\author{
  Renato Vimieiro and Carlos Riveros
}
\references{
  Landis JR, Koch GG (1977) The measurement of observer
  agreement for categorical data.  Biometrics 33: 159-174

  Fleiss JL (1971) Measuring nominal scale agreement among
  many raters. Psychol Bull 76: 378-382

  Fleiss JL, Levin B, Paik MC (2004) The Measurement of
  Interrater Agreement, John Wiley & Sons, Inc. pp.
  598-626.

  Hubert L, Arabie P (1985) Comparing partitions. Journal
  of Classification 2: 193-218.

  Vinh NX, Epps J, Bailey J (2009) Information Theoretic
  Measures for Clusterings Comparison: Is a Correction for
  Chance Necessary? In: Proceedings of the 26th Annual
  International Conference on Machine Learning. pp.
  1073-1080.
}
\seealso{
  \code{\link[irr]{kappam.fleiss}},
  \code{\link[mclust]{adjustedRandIndex}} and
  \code{\link[cibm.utils]{runWekaClassifiers}}
}

