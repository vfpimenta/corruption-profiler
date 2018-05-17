#!/usr/bin/Rscript

suppressMessages(library(rjson))
suppressMessages(library(cibm.utils))
suppressMessages(library(optparse))

date.range <- function(legislature) {
  if (legislature == 53) {
    c(1,22)
  } else if (legislature == 54) {
    c(23,70)
  } else if (legislature == 55) {
    c(71,89)
  }
}

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
    AC='#d9f0a3',    AL='#cc4c02',    AP='#78c679',    AM='#004529',
    BA='#662506',    CE='#fee391',    DF='#67000d',    ES='#dd3497',
    GO='#a50f15',    MA='#ffffe5',    MT='#cb181d',    MS='#ef3b2c',
    MG='#ae017e',    PA='#006837',    PB='#fe9929',    PR='#0570b0',
    PE='#ec7014',    PI='#fff7bc',    RJ='#7a0177',    RN='#fec44f',
    RS='#023858',    RO='#238443',    RR='#41ab5d',    SC='#045a8d',
    SP='#49006a',    SE='#993404',    TO='#addd8e'
  ) 
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

# ################
# Argument parsing
# ################
option_list <- list(
  make_option(c('-c','--dumpcl'), default=FALSE, action="store_true" ,
    dest="dumpcl", help="export clusters info to Json"),
  make_option(c('-g','--dumpgml'), default=FALSE, action="store_true",
    dest="dumpgml", help="export cluster graphs"),
  make_option(c('-s','--series'), default=NULL, type="character",
    dest="series" , help="selected series {'flight','publicity','telecom','fuels'}", metavar="character")
);

opt_parser <- OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

# #########################
# Building expense matrixes
# #########################

congressman_data <- fromJSON(file='../data/JSON/standard/congressman_ts.json')

if (!is.null(opt$series)) {
  if (opt$series == 'flight') {
    print("Using series: flight")
    congressman_data <- fromJSON(file='../data/JSON/standard/congressman_flight-ticket-issue_ts.json')
  } else if (opt$series == 'publicity') {
    print("Using series: publicity")
    congressman_data <- fromJSON(file='../data/JSON/standard/congressman_publicity-of-parliamentary-activity_ts.json')
  } else if (opt$series == 'telecom') {
    print("Using series: telecom")
    congressman_data <- fromJSON(file='../data/JSON/standard/congressman_telecommunication_ts.json')
  } else if (opt$series == 'fuels') {
    print("Using series: fuels")
    congressman_data <- fromJSON(file='../data/JSON/standard/congressman_fuels-and-lubricants_ts.json')
  } else if (opt$series == 'maintenance') {
    print("Using series: maintenance")
    congressman_data <- fromJSON(file='../data/JSON/standard/congressman_maintenance-of-office-supporting-parliamentary-activity_ts.json')
  } else if (opt$series == 'consultancy') {
    print("Using series: consultancy")
    congressman_data <- fromJSON(file='../data/JSON/standard/congressman_consultancy,-research-and-technical-work_ts.json')
  } else if (opt$series == 'auto-watercraft') {
    print("Using series: auto-watercraft")
    congressman_data <- fromJSON(file='../data/JSON/standard/congressman_automotive-vehicle-renting-or-watercraft-charter_ts.json')
  } else if (opt$series == 'auto') {
    print("Using series: auto")
    congressman_data <- fromJSON(file='../data/JSON/standard/congressman_automotive-vehicle-renting-or-charter_ts.json')
  } else if (opt$series == 'postal') {
    print("Using series: postal")
    congressman_data <- fromJSON(file='../data/JSON/standard/congressman_postal-services_ts.json')
  } else if (opt$series == 'flight-ticket') {
    print("Using series: flight-ticket")
    congressman_data <- fromJSON(file='../data/JSON/standard/congressman_flight-tickets_ts.json')
  } else if (opt$series == 'lodging') {
    print("Using series: lodging")
    congressman_data <- fromJSON(file='../data/JSON/standard/congressman_lodging,-except-for-congressperson-from-distrito-federal_ts.json')
  } else if (opt$series == 'meal') {
    print("Using series: meal")
    congressman_data <- fromJSON(file='../data/JSON/standard/congressman_congressperson-meal_ts.json')
  } else if (opt$series == 'aircraft') {
    print("Using series: aircraft")
    congressman_data <- fromJSON(file='../data/JSON/standard/congressman_aircraft-renting-or-charter-of-aircraft_ts.json')
  } else if (opt$series == 'security') {
    print("Using series: security")
    congressman_data <- fromJSON(file='../data/JSON/standard/congressman_security-service-provided-by-specialized-company_ts.json')
  } else if (opt$series == 'locomotion') {
    print("Using series: locomotion")
    congressman_data <- fromJSON(file='../data/JSON/standard/congressman_locomotion,-meal-and-lodging_ts.json')
  } else if (opt$series == 'taxi') {
    print("Using series: taxi")
    congressman_data <- fromJSON(file='../data/JSON/standard/congressman_taxi,-toll-and-parking_ts.json')
  } else if (opt$series == 'publication') {
    print("Using series: publication")
    congressman_data <- fromJSON(file='../data/JSON/standard/congressman_publication-subscriptions_ts.json')
  } else if (opt$series == 'software') {
    print("Using series: software")
    congressman_data <- fromJSON(file='../data/JSON/standard/congressman_software-purchase-or-renting;-postal-services;-subscriptions_ts.json')
  } else if (opt$series == 'office') {
    print("Using series: office")
    congressman_data <- fromJSON(file='../data/JSON/standard/congressman_purchase-of-office-supplies_ts.json')
  } else if (opt$series == 'watercraft') {
    print("Using series: watercraft")
    congressman_data <- fromJSON(file='../data/JSON/standard/congressman_watercraft-renting-or-charter_ts.json')
  } else if (opt$series == 'maritme') {
    print("Using series: maritme")
    congressman_data <- fromJSON(file='../data/JSON/standard/congressman_terrestrial,-maritime-and-fluvial-tickets_ts.json')
  } else if (opt$series == 'course') {
    print("Using series: course")
    congressman_data <- fromJSON(file='../data/JSON/standard/congressman_participation-in-course,-talk-or-similar-event_ts.json')
  }  
} else {
  print("Using series: default")
  opt$series = 'default'
}

outliers.53 <- fromJSON(file='../data/JSON/congressman_53_outliers.json')
outliers.54 <- fromJSON(file='../data/JSON/congressman_54_outliers.json')
outliers.55 <- fromJSON(file='../data/JSON/congressman_55_outliers.json')

mat.53 <- c()
mat.54 <- c()
mat.55 <- c()
mat.full <- c()

names.53 <- c()
names.54 <- c()
names.55 <- c()
names.full <- c()

for (name in names(congressman_data)){
  congressman = congressman_data[[name]]
  if (congressman[[4]][1] && !(name %in% outliers.53)) {
    range = date.range(53)
    mat.53 <- cbind(mat.53, congressman[[5]][range[1]:range[2]])
    names.53 <- c(names.53, congressman[[1]])
  } 
  if (congressman[[4]][2] && !(name %in% outliers.54)) {
    range = date.range(54)
    mat.54 <- cbind(mat.54, congressman[[5]][range[1]:range[2]])
    names.54 <- c(names.54, congressman[[1]])
  } 
  if (congressman[[4]][3] && !(name %in% outliers.55)) {
    range = date.range(55)
    mat.55 <- cbind(mat.55, congressman[[5]][range[1]:range[2]])
    names.55 <- c(names.55, congressman[[1]])
  }

  mat.full <- cbind(mat.full, congressman[[5]])
  names.full <- c(names.full, congressman[[1]])
}

colnames(mat.53) <- names.53
colnames(mat.54) <- names.54
colnames(mat.55) <- names.55
colnames(mat.full) <- names.full

# ###############
# Main processing
# ###############

dump.path <- paste('../data/', opt$series, '/dump/', sep="")
graphs.path <- paste('../data/', opt$series,'/graphs/', sep="")

if (opt$dumpcl) {
  if (dir.exists(dump.path)) {
    unlink(dump.path, recursive=TRUE, force=TRUE)
  }
}
if (opt$dumpgml) {
  if (dir.exists(graphs.path)) {
    unlink(graphs.path, recursive=TRUE, force=TRUE)
  }
}

idx <- 53
for (mat in list(mat.53, mat.54, mat.55)) {
  for (method in list("robust", "JS", "cosine")) {
    for (k in list(2,3,4,5)) {
      d <- distance(mat, method=method)
      gmstknn <- mstknn(d, k=k)
      cls <- clusters(gmstknn)

      if (opt$dumpcl) {
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

        dump.path <- paste('../data/', opt$series, '/dump/', method, '/k-', k, '/', sep="")
        dir.create(dump.path, recursive=TRUE)
        export <- toJSON(cluster.list)
        file <- paste(dump.path, 'dump-clusters-', idx, '.json', sep='')
        print('==============================================================')
        print(paste('Dumping json to path ', dump.path, sep=""))
        print('==============================================================')
        write(export, file)
      }

      for (congressman.id in names(congressman_data)) {
        congressman <- congressman_data[[congressman.id]]
        if (congressman[[1]] %in% colnames(mat)) {
          V(gmstknn)[congressman[[1]]]$id <- congressman.id
          V(gmstknn)[congressman[[1]]]$state <- congressman[[2]]
          V(gmstknn)[congressman[[1]]]$term_color <- color.term.node(congressman[[4]])
          V(gmstknn)[congressman[[1]]]$region <- region.node(congressman[[2]])
          V(gmstknn)[congressman[[1]]]$size <- sum(congressman[[5]])
          V(gmstknn)[congressman[[1]]]$group <- cls$membership[[congressman[[1]]]]
        }
      }

      if (opt$dumpgml) {
        graphs.path <- paste('../data/', opt$series,'/graphs/', method, '/k-', k, '/', sep="")
        dir.create(graphs.path, recursive=TRUE)
        file <- paste(graphs.path, 'cibm-regioncolor-', idx, '.graphml', sep='')
        print('==============================================================')
        print(paste('Writing graphs to path ', graphs.path, sep=""))
        print('==============================================================')
        write_graph(gmstknn, file, 'graphml')
      }
    }
  }
  idx <- idx+1
}