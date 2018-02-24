#indicator species analysis of center sub data  

#loads response matrix
cover2 <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/ccover2.csv", header = T)
#loads explan matrix
cover1 <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/cover1.csv", header = T)

SC <- cover1$SC
#indicator species analysis?
library(indicspecies)
cov.ind <- multipatt(cover2, cluster = SC)