library(rjson)
library(cibm.utils)

color.node <- function(el) {
  if (all(el==c(0,0,1))) {
    'red'
  } else if (all(el==c(0,1,0))) {
    'orange'
  } else if (all(el==c(0,1,1))) {
    'yellow'
  } else if (all(el==c(1,0,0))) {
    'green'
  } else if (all(el==c(1,0,1))) {
    'blue'
  } else if (all(el==c(1,1,0))) {
    'indigo'
  } else if (all(el==c(1,1,1))) {
    'violet'
  } else {
    NA
  }
}

region.node <- function(el) {
  if (el %in% c('RS','PR','SC')) {
    'south'
  } else if (el %in% c('SP','RJ','ES','MG')) {
    'south-east'
  } else if (el %in% c('MT','MS','GO','DF')) {
    'center-west'
  } else if (el %in% c('BA','SE','AL','PE','PB','RN','CE','PI','MA')) {
    'north-east'
  } else if (el %in% c('RO','AC','AM','RR','PA','AP','TO')) {
    'north'
  } else {
    stop(paste('Unknown state ', el, sep=""))
  }
}

congressman_data <- fromJSON(file='/home/victor/dev/corruption-profiler/data/congressman_ts.json')

mat.53 <- c()
mat.54 <- c()
mat.55 <- c()

names.53 <- c()
names.54 <- c()
names.55 <- c()

for (congressman in congressman_data){
  if (congressman[[4]][1]) {
    mat.53 <- cbind(mat.53, congressman[[5]])
    names.53 <- c(names.53, congressman[[1]])
  } 
  if (congressman[[4]][2]) {
    mat.54 <- cbind(mat.54, congressman[[5]])
    names.54 <- c(names.54, congressman[[1]])
  } 
  if (congressman[[4]][3]) {
    mat.55 <- cbind(mat.55, congressman[[5]])
    names.55 <- c(names.55, congressman[[1]])
  }
}
colnames(mat.53) <- names.53
colnames(mat.54) <- names.54
colnames(mat.55) <- names.55

# plot(gmstknn, vertex.label=NA, vertex.size=3)
# write.graph(gmstknn,file="random.cluster.gml",format="gml")

idx <- 53
for (mat in list(mat.53, mat.54, mat.55)) {
  d <- distance(mat)
  gmstknn <- mstknn(d)

  for (congressman in congressman_data) {
    if (congressman[[1]] %in% colnames(mat)) {
      V(gmstknn)[congressman[[1]]]$state <- congressman[[2]]
      V(gmstknn)[congressman[[1]]]$region <- region.node(congressman[[2]])
      V(gmstknn)[congressman[[1]]]$size <- sum(congressman[[5]])
    }
  }

  path <- paste('~/dev/corruption-profiler/data/graphs/cibm-statecolor-', idx, '.graphml',sep='')
  write_graph(gmstknn, path, 'graphml')
  idx <- idx+1
}