\name{cibm.base}
\alias{cibm.base}
\alias{cibm.base-class}
\alias{cibm.data}
\alias{cibm.data-class}
\title{CIBM's data representation}
\description{
  Virtual base class for data representation used by
  \link{cibm.data} and \link{cibm.abk}

  This class contains the data representation used by
  \link{read.nbi}
}
\section{Slots}{
  \describe{ \item{\code{data}}{A \code{data.frame}
  containing the numeric data.  Rows are features and
  columns samples} \item{\code{labels}:}{\code{factor}.
  Contains the labels of samples.} }
}

\section{Slots}{
  \describe{ \item{\code{data}}{A \code{data.frame}
  containing the numeric data.  Rows are features and
  columns samples} \item{\code{labels}:}{\code{factor}.
  Contains the labels of samples.}
  \item{\code{properties}:}{\code{data.frame}. Contains
  additional (optional) information of samples.}

  \item{\code{data}:}{(inherited) A \code{data.frame}
  containing the numeric data.  Rows are features and
  columns samples} \item{\code{labels}:}{(inherited)
  \code{factor}. Contains the labels of samples.} }
}
\author{
  Renato Vimieiro
}
\seealso{
  \link{cibm.data} \link{cibm.abk}

  \link{read.nbi} \link{cibm.base}
}

