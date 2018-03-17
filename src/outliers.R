library(rjson)
library(optparse)
library(xml2)

# Data caching
data <- read_xml('../data/Deputados.xml')
data_list <- as_list(data)

is.holder <- function(congressman_id, legislature) {
  for (congressman in data_list[[1]][[1]]){
    if (congressman[['ideCadastro']] == congressman_id && congressman[['numLegislatura']] == legislature && congressman[['Condicao']] == 'Titular' && congressman[['SiTuacaoMandato']] == 'Em Exercício') {
      return(TRUE)
    }
  }
  return(FALSE)
}

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

# ################
# Argument parsing
# ################
option_list <- list(
  make_option(c('-d','--detect'), default=FALSE, action="store_true" ,
    dest="detect", help="detect and export outliers"),
  make_option(c('-a','--analyse'), default=FALSE, action="store_true",
    dest="analyse", help="print max and min members of cluster 1 for legislature 54"),
  make_option(c('-c','--clear'), default=FALSE, action="store_true", dest="cls", help="Clear congressman with too many zeroes")
);

opt_parser <- OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

congressman_data <- fromJSON(file='../data/JSON/standard/congressman_ts.json')

valid_threshold <- 5

# #################
# Outlier detection
# #################

if(opt$detect) {
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
      } else if (!is.holder(name, legislature)) {
        outliers <- c(outliers, name)
      } else if (detect.vacancies(congressman, legislature, 3)) {
        outliers <- c(outliers, name)
      }
      # } else if (opt$cls && congressman[[4]][idx]) {
      #   expenses_head <- head(congressman[[5]][range[1]:range[2]], valid_threshold)
      #   expenses_tail <- tail(congressman[[5]][range[1]:range[2]], valid_threshold)
      #   if(sum(expenses_head == 0) >= 4 || sum(expenses_tail == 0) >= 4){
      #     outliers <- c(outliers, name)
      #   }
      # }
    }

    export <- toJSON(outliers)
    write(export, paste('../data/JSON/congressman_',legislature,'_outliers.json',sep=""))
  }
}

# ################
# Cluster analysis
# ################

if(opt$analyse) {
  congressman_clusters.robust <- fromJSON(file='../data/dump/robust/k-3/dump-clusters-54.json')
  congressman_clusters.js <- fromJSON(file='../data/dump/robust/k-3/dump-clusters-54.json')

  range <- date.range(54)

  cluster1 <- congressman_data[congressman_clusters.robust[[4]]]
  cluster1.avgs <- c()
  for (congressman in cluster1) {
    cluster1.avgs <- c(cluster1.avgs, mean(congressman[[5]][range[1]:range[2]]))
  }

  cluster1.max <- cluster1[[which.max(cluster1.avgs)]]
  cluster1.min <- cluster1[[which.min(cluster1.avgs)]]

  cluster1.max.ts <- ts(cluster1.max[[5]][range[1]:range[2]], frequency=12, start=c(2011,02))
  cluster1.min.ts <- ts(cluster1.min[[5]][range[1]:range[2]], frequency=12, start=c(2011,02))

  plot(cluster1.max.ts)
  plot(cluster1.min.ts)
}