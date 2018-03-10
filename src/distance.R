#!/usr/bin/Rscript

library(cibm.utils)
library(rjson)

date.range <- function(legislature) {
  if (legislature == 53) {
    c(1,22)
  } else if (legislature == 54) {
    c(23,70)
  } else if (legislature == 55) {
    c(71,89)
  }
}

normalize.vector <- function(vector) {
  (vector-min(vector))/(max(vector)-min(vector))
}

congressman_data <- fromJSON(file='../data/congressman_ts.json')

outliers.54 <- fromJSON(file='../data/congressman_54_outliers.json')
mat.54 <- c()
names.54 <- c()

for (name in names(congressman_data)){
  congressman = congressman_data[[name]]
  if (congressman[[4]][2] && !(name %in% outliers.54)) {
    range = date.range(54)
    mat.54 <- cbind(mat.54, congressman[[5]][range[1]:range[2]])
    names.54 <- c(names.54, name)
  }
}

colnames(mat.54) <- names.54

# norm.54 <- c()
# for (vec in 1:dim(mat.54)[1]){
#   norm.54 <- rbind(norm.54, normalize.vector(mat.54[vec,]))
# }

# colnames(norm.54) <- names.54

for (method in list("robust", "JS", "cosine")){
  d <- distance(mat.54, method=method)
  #d[is.na(d)] = 1
  dm <- as.matrix(d)

  write.csv(dm, file=paste('distance-', method, '.csv', sep=""))
}