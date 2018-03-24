#!/usr/bin/Rscript

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


outliers.54 <- fromJSON(file='../data/JSON/congressman_54_outliers.json')
ts.files <- list.files(path='../data/JSON/standard/', pattern='ts.json')
for (file in ts.files){
  congressman.data <- fromJSON(file=paste('../data/JSON/standard/', file, sep=""))
  iq.mat <- list()
  iq.list <- list()
  range <- date.range(54)

  for (name in names(congressman.data)) {
    congressman <- congressman.data[[name]]
    if (congressman[[4]][2] && !(name %in% outliers.54) & mean(congressman[[5]]) > 0) {
      iq.mat <- rbind(iq.mat, congressman[[5]][range[1]:range[2]])
    }
  }

  for (i in 1:(range[2]-range[1]+1)) {
    iq.proportion <- boxplot(unlist(iq.mat[,i]), plot=FALSE)
    iq.list <- c(iq.list, iq.proportion$stats[4] - iq.proportion$stats[2])
  }


  # std.list <- list()
  # for (name in names(congressman.data)) {
  #   congressman <- congressman.data[[name]]
  #   range <- date.range(54)
  #     if (congressman[[4]][2] && !(name %in% outliers.54) & mean(congressman[[5]][range[1]:range[2]]) > 0) {
  #       std.proprtion <- sd(congressman[[5]][range[1]:range[2]]) / mean(congressman[[5]][range[1]:range[2]])
  #       std.list <- c(std.list, std.proprtion)
  #     }
  # }

  #mu <- mean(unlist(std.list))
  mu <- mean(unlist(iq.list))
  print(paste("File:", file, " Average std:", mu))
}