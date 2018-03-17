#!/usr/bin/Rscript

library(rjson)

outliers.54 <- fromJSON(file='../data/JSON/congressman_54_outliers.json')

ts.files <- list.files(path='../data/JSON/standard/', pattern='ts.json')
for (file in ts.files){
	congressman.data <- fromJSON(file=paste('../data/JSON/standard/', file, sep=""))
	std.list <- list()

	for (name in names(congressman.data)) {
		congressman <- congressman.data[[name]]
  		if (congressman[[4]][2] && !(name %in% outliers.54) & mean(congressman[[5]]) > 0) {
        std.proprtion <- sd(congressman[[5]]) / mean(congressman[[5]])
  			std.list <- c(std.list, std.proprtion)
  		}
	}

	mu <- mean(unlist(std.list))
	print(paste("File:", file, " Average std:", mu))
}