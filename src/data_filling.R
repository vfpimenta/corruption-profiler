#!/usr/bin/Rscript

library(rjson)
library(Amelia)

date.range <- function(legislature) {
  if (legislature == 53) {
    c(1,22)
  } else if (legislature == 54) {
    c(23,70)
  } else if (legislature == 55) {
    c(71,89)
  }
}

date.seq <- function(legislature) {
  if (legislature == 53) {
    seq(as.Date("2009-04-01"), as.Date("2011-01-31"), by="month")
  } else if (legislature == 54) {
    seq(as.Date("2011-02-01"), as.Date("2015-01-31"), by="month")
  } else if (legislature == 55) {
    seq(as.Date("2015-02-01"), as.Date("2016-08-31"), by="month")
  }
}

detect.vacancies <- function(data, legislature, threshold){
  range <- date.range(legislature)
  watch.list <- data[[5]][range[1]:range[2]]
  zeroes.count <- 0
  zeroes.sequence <- 0
  for (value in watch.list) {
    if (value == 0){
      zeroes.count <- zeroes.count + 1
    } else {
      if (zeroes.count > zeroes.sequence) {
        zeroes.sequence <- zeroes.count
      }
      zeroes.count <- 0
    }
  }

  return(zeroes.sequence >= threshold)
}

congressman.data <- fromJSON(file='../data/congressman_ts.json')
gap.threshold <- 9
legislature <- 54

for (name in names(congressman.data)) {
  congressman <- congressman.data[[name]]
  has.gap <- detect.vacancies(congressman, legislature, 1)
  has.biggap <- detect.vacancies(congressman, legislature, gap.threshold)

  if (has.gap && !has.biggap) {
    range <- date.range(legislature)
    congressman.list <- congressman[[5]][range[1]:range[2]]
    congressman.list[congressman.list == 0] <- NA
    congressman.date <- date.seq(legislature)
    congressman.df <- data.frame(congressman.list, congressman.date)
    congressman.out <- amelia(congressman.df, m=5)

    congressman[[5]][range[1]:range[2]] <- congressman.out$imputations[[3]]$congressman.list
  }

  congressman.data[[name]] <- congressman
}

export <- toJSON(congressman.data)
write(export, '../data/congressman_filled_ts.json')