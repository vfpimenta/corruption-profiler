library("cibm.utils")

data("alzheimer",package="cibm.utils")

alzheimer[c(1:6,121),1:9]

# Write data to NBI formatted file -- Notice by default the file is gzipped
write.nbi(alzheimer,file="alzheimer.nbi",gzip=FALSE)
