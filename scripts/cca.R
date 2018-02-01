#performs a CCA, using fire severity in order to constrain the ordination

#loads response matrix
cover2 <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/cover2.csv", header = T)
#loads explan matrix
cover1 <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/cover1.csv", header = T)

library(vegan)

cover.stand <- decostand(cover1[,3:4], "range")

cca.cover <- cca(X=cover2, Y=cover.stand)

plot(cca.cover)
