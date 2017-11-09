#!/usr/bin/Rscript
# Formatting according to https://google.github.io/styleguide/Rguide.xml

suppressMessages(library(base))
suppressMessages(library(tseries))
suppressMessages(library(forecast))
suppressMessages(library(hydroGOF))
suppressMessages(library(optparse))

range01 <- function(x){
  (x-min(x))/(max(x)-min(x))
}

add.months <- function(date,n) {
  seq(date, by = paste (n, "months"), length = 2)[2]
}

get.month <- function(date) {
  as.integer(substr(date,6,7))
}

get.year <- function(date) {
  as.integer(substr(date,1,4))
}

# ################
# Argument parsing
# ################
option_list <- list(
  make_option(c('-t','--type'), type="character", default=NULL, 
    help="execution type {'sample','full'}", metavar="character"),
  make_option(c('-o','--offset'), type="integer", default=1,
    help="starting index used when type = 'full'", metavar="number")
);

opt_parser <- OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

if (length(opt$type) == 0){
  stop('No option selected')
}

# ############
# Data reading
# ############
data <- read.csv('Monthly expenses by congressman in Brazil 2009-2016.csv')
expenses <- data$Expenses
expenses.length <- length(expenses)
expenses.start <- data$Date[1]
expenses.start.year <- get.year(expenses.start)
expenses.start.month <- get.month(expenses.start)
expenses.ts <- ts(expenses,frequency=12,
  start=c(expenses.start.year,expenses.start.month))

expenses.norm <- range01(expenses)
expenses.norm.ts <- ts(expenses.norm,frequency=12,
  start=c(expenses.start.year,expenses.start.month))

# ###############
# Main processing
# ###############
if (opt$type == 'sample') {
  expenses.norm.ts_hold <- ts(expenses.norm,frequency=12, 
    start=c(2009,7),end=c(2015,12))
  expenses.norm.arima <- auto.arima(expenses.norm.ts_hold)
  expenses.norm.forecast <- forecast(expenses.norm.arima, h=8)

  diff <- mse(expenses.norm.forecast$mean, expenses.norm.ts[79:86])
  cat('final mse:',diff,'\n')

  plot(expenses.norm.forecast)
  lines(expenses.norm.ts)
} else if (opt$type == 'full') {
  plot(expenses.norm.ts)

  forecasts <- c()
  mses <- c()
  for (i in (opt$offset+1):expenses.length) {
    expenses.norm.ts_hold <- ts(expenses.norm[1:i-1],frequency=12,
      start=c(expenses.start.year,expenses.start.month))
    expenses.norm.arima <- auto.arima(expenses.norm.ts_hold)
    expenses.norm.forecast <- forecast(expenses.norm.arima, h=1)

    diff <- mse(expenses.norm.forecast$mean, expenses.norm.ts[i])
    mses <- c(mses, diff)
    forecasts[i-opt$offset] = expenses.norm.forecast$mean
    print(diff)

    points(expenses.norm.forecast$mean, pch=4, col='red')
  }
  expenses.start.date <- as.Date(
    paste(expenses.start.year,expenses.start.month,1),format='%Y %m %d')

  forecasts.date <- add.months(expenses.start.date,opt$offset)
  forecasts.year <- get.year(forecasts.date)
  forecasts.month <- get.month(forecasts.date)
  forecasts.ts <- ts(forecasts, frequency=12, 
    start=c(forecasts.year,forecasts.month))
  lines(forecasts.ts, col='red')

  diff <- mse(forecasts, expenses.norm.ts[(opt$offset+1):expenses.length])
  cat('final mse:',diff,'\n')

  plot(mses,type='l')
}