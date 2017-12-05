\name{predict_XVal}
\alias{predict_XVal}
\title{Predict class labels in a cross-validation setting}
\usage{
  predict_XVal(classifier, numFolds,
    seed = as.integer(Sys.time()))
}
\arguments{
  \item{classifier}{\code{\link[RWeka]{Weka_classifiers}}.
  The classifier to use in cross-validation.}

  \item{numFolds}{\code{integer}.}

  \item{seed}{\code{integer}. Random seed to be used in
  cross-validation. Default: current time.}
}
\value{
  \code{data.frame} Containing: the 'actual' label; the
  'predicted' label; 'error' indicator; and 'prediction'
  reliability
}
\description{
  Partition data in k-folds, then create model using k-1
  folds and predict labels for samples in remaining folds
  using the model. Finally returns all labels.
}

