\name{cibm.abk}
\alias{cibm.abk}
\alias{cibm.abk-class}
\title{CIBM's Alfa-Beta-k data representation}
\description{
  This class contains the data representation used by
  \link{read.abk} This class inherits from the virtual
  class \code{cibm.base} class (see
  \link{cibm.data-class}).
}
\section{Slots}{
  \describe{ \item{\code{data}:}{A \code{data.frame}
  containing the numeric data.  Rows are features and
  columns samples} \item{\code{labels}:}{\code{factor}.
  Contains the labels of samples.}
  \item{\code{caseAttr}:}{\code{data.frame}. Contains
  additional (optional) information of samples.  'weights'
  and 'colours' are defined as 1 by default}
  \item{\code{featureAttr}:}{\code{data.frame}. Contains
  additional (optional) information of features.  'weights'
  and 'colours' are defined as 1 by default}
  \item{\code{beta}:}{vector of beta values used as target}

  \item{\code{data}:}{(inherited) A \code{data.frame}
  containing the numeric data.  Rows are features and
  columns samples} \item{\code{labels}:}{(inherited)
  \code{factor}. Contains the labels of samples.} }
}
\author{
  Carlos Riveros
}
\seealso{
  \link{read.abk} \link{write.abk} \link{cibm.base}
}

