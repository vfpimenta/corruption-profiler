# loads library
library("cibm.utils")

# defines filename
filename <- system.file("extdata","2695.abk",package="cibm.utils")

# reads
a2695 <- read.abk(filename)

# prints -- Notice that class is numeric
a2695[c(1:6,100),1:9]


