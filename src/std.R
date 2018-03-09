library(rjson)

outliers.54 <- fromJSON(file='../data/congressman_54_outliers.json')

ts.files <- list.files(path='../data', pattern='ts.json')
for (file in ts.files){
	congressman.data <- fromJSON(file=paste('../data/', file, sep=""))
	std.list <- list()

	for (name in names(congressman.data)) {
		congressman <- congressman.data[[name]]
  		if (congressman[[4]][2] && !(name %in% outliers.54)) {
  			std.list <- c(std.list, sd(congressman[[5]]))
  		}
	}

	mu <- mean(unlist(std.list))
	print(paste("File:", file, " Average std:", mu))
}