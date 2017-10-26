library(rjson)
library(cibm.utils)

congressman_data <- fromJSON(file='/home/victor/dev/corruption-profiler/data/congressman_ts.json')

mat <- c()
names <- c()
for (congressman in congressman_data){
	mat <- cbind(mat, congressman[[5]])
	names <- c(names, congressman[[1]])
}
colnames(mat) <- names

d <- distance(mat, method='robust')
gmstknn <- mstknn(d)

# plot(gmstknn, vertex.label=NA, vertex.size=3)
# write.graph(gmstknn,file="random.cluster.gml",format="gml")

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

for (congressman in congressman_data) {
	V(gmstknn)[congressman[[1]]]$color <- color.node(congressman[[4]])
}

write_graph(gmstknn, '~/dev/corruption-profiler/data/cibm-color.graphml', 'graphml')