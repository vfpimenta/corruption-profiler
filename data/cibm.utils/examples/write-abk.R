library("cibm.utils")

data("a2695",package="cibm.utils")

a2695[c(1:6,12),1:9]

# Write data to NBI formatted file -- Notice by default the file is gzipped
write.abk(a2695,file="a2695.abk",classes="first",out.equalweights=T,gzip=F)