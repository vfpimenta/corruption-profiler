\name{CM1}
\alias{CM1}
\alias{CM2}
\title{CM functions}
\usage{
  CM1(.data, .labels = NULL, .parallel = FALSE)

  CM2(.data, .labels)
}
\arguments{
  \item{.data}{a \code{\link{data.frame}} or
  \code{\link{cibm.data}}; rows are features, columns
  samples}

  \item{.labels}{\code{\link{factor}} containing the class
  labels of samples;}

  \item{.parallel}{\code{logical} Should CM1 of each class
  be computed in parallel?}
}
\value{
  \item{\code{\link{data.frame}}}{one column per label and
  one row per feature}
}
\description{
  Computes CM values of features
}
\details{
  These functions compute the CM statistics for a dataset.
  \eqn{CM_1(f,X,Y)=\frac{\mu^f_X - \mu^f_Y}{1 + max(y_f) -
  min(y_f)}}{ CM_1(f,X,Y)=(\mu^f_X - \mu^f_Y)/(1 + max(y_f)
  - min(y_f))} and \eqn{CM_2(f,X,Y)=\frac{\mu^f_X -
  \mu^f_Y}{1 + max(y_f) - min(y_f)}}{ CM_2(f,X,Y)=(\mu^f_X
  - \mu^f_Y)/(1 + min(max(y_f) - min(y_f),max(x_f) -
  min(x_f))}.

  The parameter of this function can either be a
  \code{data.frame} or a \code{cibm.data}. In the first
  case, \code{labels} must be specified. It must be a
  \code{factor}.
}
\examples{
\dontrun{
data(alzheimer)
cm1_alzheimer <- CM1(alzheimer)
head(cm1_alzheimer)
}
}
\author{
  Renato Vimieiro
}

