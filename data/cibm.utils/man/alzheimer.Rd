\name{alzheimer}
\alias{alzheimer}
\docType{data}
\title{
An example dataset
}
\description{
Contains the microarray gene expression data on Alzheimer's Disease presented by Ray et al. (2007).
}
\usage{data(alzheimer)}
\format{
  A data frame with 121 features (rows) on the following 83 samples (columns).
}
\source{
\url{http://www.nature.com/nm/journal/v13/n11/full/nm1653.html}
}
\references{
Ray S, Britschgi M, Herbert C, Takeda-Uchimura Y, Boxer A, et al. (2007) Classification and prediction of clinical Alzheimer's diagnosis based on plasma signaling proteins. Nat Med 13: 1359-1362. \href{http://dx.doi.org/10.1038/nm1653}{doi:10.1038/nm1653}.

Gomez Ravetti M, Moscato P (2008) Identification of a 5-Protein Biomarker Molecular Signature for Predicting Alzheimer's Disease. PLoS ONE 3(9): e3111. \href{http://dx.doi.org/10.1371/journal.pone.0003111}{doi:10.1371/journal.pone.0003111}
}
\examples{
data(alzheimer)
alzheimer[1:10,1:9]
}
\keyword{datasets}
