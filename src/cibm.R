library(rjson)
library(cibm.utils)

color.term.node <- function(el) {
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

color.state.node <- function(el) {
  switch(el,
    AC='#d9f0a3',
    AL='#cc4c02',
    AP='#78c679',
    AM='#004529',
    BA='#662506',
    CE='#fee391',
    DF='#67000d',
    ES='#dd3497',
    GO='#a50f15',
    MA='#ffffe5',
    MT='#cb181d',
    MS='#ef3b2c',
    MG='#ae017e',
    PA='#006837',
    PB='#fe9929',
    PR='#0570b0',
    PE='#ec7014',
    PI='#fff7bc',
    RJ='#7a0177',
    RN='#fec44f',
    RS='#023858',
    RO='#238443',
    RR='#41ab5d',
    SC='#045a8d',
    SP='#49006a',
    SE='#993404',
    TO='#addd8e') 
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
mat.full <- c()

names.53 <- c()
names.54 <- c()
names.55 <- c()
names.full <- c()

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

  mat.full <- cbind(mat.full, congressman[[5]])
  names.full <- c(names.full, congressman[[1]])
}
colnames(mat.53) <- names.53
colnames(mat.54) <- names.54
colnames(mat.55) <- names.55
colnames(mat.full) <- names.full

# plot(gmstknn, vertex.label=NA, vertex.size=3)
# write.graph(gmstknn,file="random.cluster.gml",format="gml")

idx <- 53
for (mat in list(mat.53, mat.54, mat.55)) {
  d <- distance(mat)
  gmstknn <- mstknn(d)

  cls <- clusters(gmstknn)
  cluster.list <- list()
  for (i in 1:length(congressman_data)) {
    congressman <- congressman_data[[i]]
    if(congressman[[1]] %in% colnames(mat)) {
      cluster.index <- cls$membership[[congressman[[1]]]]
      if(cluster.index <= length(cluster.list)) {
        cluster.list[[cluster.index]] <- c(cluster.list[[cluster.index]], names(congressman_data)[i])
      } else {
        cluster.list[[cluster.index]] <- names(congressman_data)[i]
      }
    }
  }

  export <- toJSON(cluster.list)
  path <- paste('~/dev/corruption-profiler/data/dump-clusters-', idx, '.json',sep='')
  write(export, path)

  # for (congressman in congressman_data) {
  #   if (congressman[[1]] %in% colnames(mat)) {
  #     V(gmstknn)[congressman[[1]]]$state <- congressman[[2]]
  #     V(gmstknn)[congressman[[1]]]$state_color <- color.state.node(congressman[[2]])
  #     V(gmstknn)[congressman[[1]]]$region <- region.node(congressman[[2]])
  #     V(gmstknn)[congressman[[1]]]$size <- sum(congressman[[5]])
  #   }
  # }

  # path <- paste('~/dev/corruption-profiler/data/graphs/cibm-regioncolor-', idx, '.gml',sep='')
  # write_graph(gmstknn, path, 'gml')
  idx <- idx+1
}

# d <- distance(mat.full)
# gmstknn <- mstknn(d)

# for (congressman in congressman_data) {
#   V(gmstknn)[congressman[[1]]]$state <- congressman[[2]]
#   V(gmstknn)[congressman[[1]]]$state_color <- color.state.node(congressman[[2]])
#   V(gmstknn)[congressman[[1]]]$term_color <- color.term.node(congressman[[4]])
#   V(gmstknn)[congressman[[1]]]$region <- region.node(congressman[[2]])
#   V(gmstknn)[congressman[[1]]]$size <- sum(congressman[[5]])
# }

# write_graph(gmstknn, '~/dev/corruption-profiler/data/graphs/cibm-full.graphml', 'graphml')