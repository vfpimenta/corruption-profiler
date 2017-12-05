# loads library
library("cibm.utils")

# defines filename
filename <- system.file("extdata","alzheimer.nbi",package="cibm.utils")

# reads
alzheimer <- read.nbi(filename)

# prints -- Notice that class is numeric
alzheimer[c(1:6,121),1:9]


