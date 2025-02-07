# read nursing home data from Oxford cite
# 
nrshome <- read.table("ch12.dat", header = TRUE)

save(nrshome, file = "nrshome.Rdata")

