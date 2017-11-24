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

box.range <- function(data) {
  for (r in 1:150/100) {
    if(length(boxplot(data, plot=FALSE, range=r)$out) <= round(length(data)/10)) {
      return(r)
    }
  }
  return(NULL)
}

congressman_data <- fromJSON(file='/home/victor/dev/corruption-profiler/data/congressman_ts.json')
congressman_clusters <- fromJSON(file='/home/victor/dev/corruption-profiler/data/dump/robust/k-3/dump-clusters-54.json')

avg.expenses <- c()
idx <- 0
for (legislature in c(53,54,55)){
  idx <- idx + 1
  range <- date.range(legislature)

  for (congressman in congressman_data) {
    if (congressman[[4]][idx]) {
      avg.expenses <- c(avg.expenses, mean(congressman[[5]][range[1]:range[2]]))
    }
  }

  outliers <- c()
  outliers.expenses <- boxplot(avg.expenses, range=box.range(avg.expenses))$out

  for (name in names(congressman_data)) {
    congressman <- congressman_data[[name]]
    if (congressman[[4]][idx] && mean(congressman[[5]][range[1]:range[2]]) %in% outliers.expenses) {
      outliers <- c(outliers, name)
    }
  }

  export <- toJSON(outliers)
  write(export, paste('/home/victor/dev/corruption-profiler/data/congressman_',legislature,'_outliers.json',sep=""))
}

# cluster1 <- congressman_data[congressman_clusters[[1]]]
# cluster1.avgs <- c()
# for (congressman in cluster1) {
#   cluster1.avgs <- c(cluster1.avgs, mean(congressman[[5]][range[1]:range[2]]))
# }

# cluster1.max <- cluster1[[which.max(cluster1.avgs)]]
# cluster1.min <- cluster1[[which.min(cluster1.avgs)]]

# cluster1.max.ts <- ts(cluster1.max[[5]][range[1]:range[2]], frequency=12, start=c(2011,02))
# cluster1.min.ts <- ts(cluster1.min[[5]][range[1]:range[2]], frequency=12, start=c(2011,02))

# plot(cluster1.max.ts)
# plot(cluster1.min.ts)