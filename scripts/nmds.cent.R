#For NMDS using center sub plot data

#loads response matrix - absolute cover for each spp
cover2 <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/ccover2.csv", header = T)

#loads explan matrix (rdnbr, severity combos)
cover1 <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/ccover1.csv", header = T)

#creates vector for severity combo category
cat <- cover1$SC

#relativize cover data based on row max and plot total
#cover2.stand <- wisconsin(cover2)

#adds dummy species with cover=.01 for BC distance meaure
cover2$fake <- .01
#relativize cover data
cover2.Rel <- decostand(cover2, method = "total")

#NMDS
library(vegan)

#MDS
z <- metaMDS(comm = cover2, distance = "bray", k=2, weakties=T, trymax = 50)

stressplot(z, lwd=2)
z$stress

#plots the ordination
plot(z, display="sites", type = "n")
#adds points, color-coded to severity combo
points(z, col = cat, pch=20)

#rotates axes according to storrie severity (continuous)
z.sev <- MDSrotate(z, cover1$storrie_rdnbr)
plot(z.sev, display = "sites")
points(z.sev$points, col = cat, pch=16)

