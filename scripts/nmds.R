#for nmds using midstory

#loads response matrix
cover2 <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/cover2.csv", header = T)
#loads explan matrix
cover1 <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/cover1.csv", header = T)

#NMDS
library(vegan)
z <- metaMDS(comm = cover2, distance = "bray", k=2, weakties=T)

stressplot(z, lwd=2)

#plots nmds
plot(z, type="n")
points(z, display = "sites")

#overlays dominant species name onto plot
text(domin, labels=cov)

#overlays severity values onto plot
text(z, labels=st, col="red")
text(z, labels=ch, col="green", pos=4)


#uses severity to rotate plot
MDSrotate()


