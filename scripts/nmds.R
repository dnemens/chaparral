#for nmds using midstory

#loads response matrix
cover2 <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/cover2.csv", header = T)
#loads explan matrix
cover1 <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/cover1.csv", header = T)

#NMDS
library(vegan)

#remove dummy species
cover2 <- cover2[,-c(8)]
#relativize cover data based on row max and plot total
cover2.stand <- wisconsin(cover2)
#relativize predictor data by max val??
cover1.stand

#MDS
z <- metaMDS(comm = cover2, distance = "bray", k=2, weakties=T)

stressplot(z, lwd=2)

#hierarchical clustering?
#creates dissimilarity matrix
cover2.dist <- vegdist(cover2.stand)
#clusters
cover.hclust <- hclust(d=cover2.dist, method = "ward.D2")
plot(cover.hclust)
g3 <- cutree(cover.hclust, k=3)

#overlay clusters on NMDS
#plots nmds
plot(z, display="sites")
ordicluster(ord=z, display = "sites", cluster=cover.hclust, prune=2)
ordihull(ord=z, groups = g3, col=c("blue", "green", "red"), lwd=2)

#overlays severity on nmds?
sev.fit <- envfit(z ~ storrie_rdnbr + chips_rdnbr, data=cover1)
plot(z, display = "sites")
plot(sev.fit)

#rotate with severity
z.sev <- MDSrotate(z, cover1$storrie_rdnbr)
plot(z.sev, display = "sites")
text(domin, labels = cov) ###????

SC <- cover1$SC
#indicator species analysis?
library(indicspecies)
cov.ind <- multipatt(cover2, cluster = SC)
  
  
#similarity percentage?
cov.simp <- simper(cover2, cov, permutations = 25)


#k means clustering?  
k7 <- kmeans(cover2, centers = 7, nstart = 100)
ck2 <- cascadeKM(cover2, inf.gr = 2, sup.gr = 7)
plot(ck2)

k3 <- kmeans(cover2, centers = 3)
library(cluster)
k3.pam <- pam(x=cover2, metric = "euclidean", k=3)

points(z, pch=k3$clustering, col=k3.pam$clustering, cex=2)

points 
#overlays dominant species name onto plot
domin <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/domin.csv", header = T)
cov <- domin$abun 
text(domin, labels=cov)

#overlays severity values onto plot
text(z, labels=st, col="red")
text(z, labels=ch, col="green", pos=4)



