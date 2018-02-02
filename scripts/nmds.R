#for nmds using midstory

#loads response matrix
cover2 <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/cover2.csv", header = T)
#loads explan matrix
cover1 <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/cover1.csv", header = T)

#NMDS
library(vegan)

#remove dummy species
cover2 <- cover2[,-c(8)]
#relativize data based on row max and plot total
cover2.stand <- wisconsin(cover2)

#MDS
z <- metaMDS(comm = cover2, distance = "bray", k=2, weakties=T)

stressplot(z, lwd=2)

#hierarchical clustering?
cover2.dist <- vegdist(cover2.stand)
cover.hclust <- hclust(d=cover2.dist, method = "ward.D2")
plot(cover.hclust)
g3 <- cutree(cover.hclust, k=3)

#overlay clusters on NMDS
#plots nmds
plot(z, display="sites")
ordicluster(ord=z, display = "sites", cluster=cover.hclust, prune=2)
ordihull(ord=z, groups = g3, col="blue", lwd=2)

#k means clustering?  
k7 <- kmeans(cover2, centers = 7, nstart = 100)
ck2 <- cascadeKM(cover2, inf.gr = 2, sup.gr = 7)
plot(ck2)

k3 <- kmeans(cover2, centers = 3)
library(cluster)
k3.pam <- pam(x=cover2, metric = "euclidean", k=3)


#points(z, display = "sites")

points(z, pch=k3$clustering, col=k3.pam$clustering, cex=2)

points 
#overlays dominant species name onto plot
text(domin, labels=cov)

#overlays severity values onto plot
text(z, labels=st, col="red")
text(z, labels=ch, col="green", pos=4)


#uses severity to rotate plot
MDSrotate()


