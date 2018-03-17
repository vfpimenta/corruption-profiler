library(rjson)

range01 <- function(x){
  if (max(x)-min(x) ==  0){
    0
  } else {
    (x-min(x))/(max(x)-min(x))
  }
}

num.columns <- function(matrix){
  dim(matrix)[2]
}

num.rows <- function(matrix){
  dim(matrix)[1] 
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

print.all <- function(data, legislature){
  list <- c()
  range <- date.range(legislature)

  for (name in names(data)) {
    congressman <- data[[name]]
    if(congressman[[4]][2]){
      series <- congressman[[5]][range[1]:range[2]]
      list <- cbind(list, series)
    }
  }

  plot(NULL, xlim=c(0,50), ylim=c(min(list), max(list)))
  for (i in 1:num.columns(list)){
    lines(list[,i], col="lightblue")
  }

  lines(rowMeans(list), lwd=3)
}

path <- '../data/JSON/standard/'
ts.files <- list.files(path=path, pattern='_ts.json')
norm.files <- list.files(path=gsub("standard","norm",path), pattern='.json')

if (length(ts.files) - length(norm.files) >= 2) {
  for (file in ts.files) {
    print(paste("Running normalizer for ", file))
    congressman_data <- fromJSON(file=paste('../data/JSON/standard/',file,sep=""))

    mat.53 <- c()
    mat.54 <- c()
    mat.55 <- c()

    names.53 <- c()
    names.54 <- c()
    names.55 <- c()

    for (name in names(congressman_data)){
      congressman <- congressman_data[[name]]
      if (congressman[[4]][1]) {
        range <- date.range(53)
        mat.53 <- cbind(mat.53, congressman[[5]][range[1]:range[2]])
        names.53 <- c(names.53, congressman[[1]])
      } 
      if (congressman[[4]][2]) {
        range <- date.range(54)
        mat.54 <- cbind(mat.54, congressman[[5]][range[1]:range[2]])
        names.54 <- c(names.54, congressman[[1]])
      } 
      if (congressman[[4]][3]) {
        range <- date.range(55)
        mat.55 <- cbind(mat.55, congressman[[5]][range[1]:range[2]])
        names.55 <- c(names.55, congressman[[1]])
      }
    }

    colnames(mat.53) <- names.53
    colnames(mat.54) <- names.54
    colnames(mat.55) <- names.55

    for (i in 1:num.rows(mat.53)) {
      mat.53[i,] <- range01(mat.53[i,])
    }

    for (i in 1:num.rows(mat.54)) {
      mat.54[i,] <- range01(mat.54[i,])
    }

    for (i in 1:num.rows(mat.55)) {
      mat.55[i,] <- range01(mat.55[i,])
    }

    for (name in names(congressman_data)){
      congressman <- congressman_data[[name]]
      if (congressman[[4]][1]) {
        range <- date.range(53)
        norm <- mat.53[colnames(mat.53) == congressman[[1]]]
        congressman[[5]][range[1]:range[2]] <- norm
      } 
      if (congressman[[4]][2]) {
        range <- date.range(54)
        norm <- mat.54[colnames(mat.54) == congressman[[1]]]
        congressman[[5]][range[1]:range[2]] <- norm
      } 
      if (congressman[[4]][3]) {
        range <- date.range(55)
        norm <- mat.55[colnames(mat.55) == congressman[[1]]]
        congressman[[5]][range[1]:range[2]] <- norm
      }
      congressman_data[[name]] <- congressman
    }

    print("Writing norm data")
    export <- toJSON(congressman_data)
    write(export, gsub("/standard/", "/norm/", gsub(".json", "_NORM.json", paste('../data/JSON/standard/', file, sep=""))))
  }
}

norm.expenses <- list()
for (file in norm.files) {
  congressman_data <- fromJSON(file=paste('../data/JSON/norm/',file,sep=""))
  for (name in names(congressman_data)){
    congressman <- congressman_data[[name]]
    if (name %in% names(norm.expenses)){
      norm.expenses[[name]] <- norm.expenses[[name]] + congressman[[5]]
    } else {
      norm.expenses[[name]] <- congressman[[5]]
    }
  }
}

congressman_data <- fromJSON(file='../data/JSON/standard/congressman_ts.json')
for (name in names(congressman_data)){
  congressman <- congressman_data[[name]]
  congressman[[5]] <- norm.expenses[[name]]
  congressman_data[[name]] <- congressman
}

export <- toJSON(congressman_data)
write(export, '../data/JSON/norm/congressman_ts_NORM.json')