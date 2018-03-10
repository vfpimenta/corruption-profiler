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

congressman_data <- fromJSON(file='../data/congressman_ts.json')
proportional_data <- congressman_data

for (name in names(congressman_data)){
  congressman <- congressman_data[[name]]
  proportional_segment <- list()
  for (r in c(53, 54, 55)){
    range <- date.range(r)
    segment <- congressman[[5]][range[1]:range[2]]
    for (value in segment){
      if (sum(segment) > 0){
        prop <- value / sum(segment)
      } else {
        prop <- 0
      }

      if (length(proportional_segment) >= r) {
        proportional_segment[[r]] <- c(proportional_segment[[r]], prop)
      } else {
        proportional_segment[[r]] <- c(prop)
      }
    }
  }
  proportional_data[[name]][[5]] <- c(
    proportional_segment[[53]],
    proportional_segment[[54]],
    proportional_segment[[55]])
}

export <- toJSON(proportional_data)
file <- '../data/congressman_pts.json'
write(export, file)