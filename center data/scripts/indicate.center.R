#indicator species analysis of center sub data  
library(tidyverse)
library(vegan)

#data frame of response variables (ALL common species Relative cover values)
cover2 <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/coverRel.csv")
# dataframe of predictor variables (rdnbr, plot names & categories)
cover <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/cover1.csv")

#creates vector of storrie/chips severity combination
cat <- as.factor(cover$SC)
##############################################################################################
#indicator species analysis using field-determined severity combinations####
library(indicspecies)
cov.ind <- multipatt(cover2, cluster = cat, control = how(nperm = 999), duleg = T)
summary(cov.ind, indvalcomp = T)

#extracts indicator value for each significant species
library(labdsv)
ind1 <- indval(cover2, cat)
ind1$indval
summary(ind1, alpha=1)

#check correlation usings Pearson's Phi
#convert cover data to a presence/absence matrix
cover2pa <- as.data.frame(ifelse(cover2>0, 1, 0))
phi <- multipatt(cover2pa, cat, func = "r.g", control=how(nperm=999), duleg = T)
summary(phi)
############################################################################################
#combine indicator species analysis with k means clustering####
#choose # of groups

ck <- cascadeKM(cover2, inf.gr = 2, sup.gr = 10) #evaluates # of groups 
plot(ck) #indicates that 4 groups are best?

#conduct a kmeans cluster analysis
k <- kmeans(cover2, centers = 4, nstart = 100)

#runs combined analysis 
#indicator species analysis with kmeans cluster determining groups
kind <- multipatt(cover2, k$cluster, control = how(nperm = 999))
summary(kind, indvalcomp = TRUE)
kind.val <- indval(cover2, k$cluster)
summary(kind.val)

#should I use the pam function, since the distance matrix above may not be appropriate?  
library(cluster)
#create a BC distance matrix for analysis
cov.dist <- vegdist(cover2, method = "bray")
asw <- numeric(10)
for (k in 2:10) {
  asw[k] <- pam(cov.dist, k = k) $ silinfo $ avg.width
}
cat("silhouette-optimal # of clusters:", which.max(asw), "\n")
k6.pam <- pam(x=cov.dist, k=6)
summary(k6.pam)
k6.pam$clustering
###############################################################
#creates NMDS ordination####
z <- metaMDS(comm = cover2, k=3, distance = "horn", weakties=T, trymax = 100, autotransform = FALSE)
z$stress

#ordination using cluster analysis to determine groups (not severity)####
z.cluster <- MDSrotate(z, cover$storrie_rdnbr)
plot(z.cluster, display = "sites")
ordihull(z.cluster, groups = k$cluster, col = c("red", "green", "blue", "orange"))
text(z.cluster, labels = kind$cluster)
cluster.fit <- envfit(z.cluster ~ storrie_rdnbr + chips_rdnbr, data=cover, na.rm=T)
plot(cluster.fit)
clus <- as.factor(kind$cluster)

ex1 <- mrpp(cover2, grouping = k$cluster, distance = "bray") #A: 0.5729 

################################################################
#rotate NMDS by storrie severity####
z.sev <- MDSrotate(z, cover$storrie_rdnbr)
plot(z.sev, display = "sites")
#code points by cluster from kmeans
points(z.sev$points, col=k$cluster, pch=20, cex=2)
#text(z.sev, labels = cover$SC)
#text(z.sev, labels=dom$abun, cex=1.2, col="black") 
#ordihull(z.sev, groups = k$cluster, col=c("black", "red", "green", "blue"))
title(main = "NMS with Dominant species overlaid")

#add continuous severity vectors
sev.fit <- envfit(z.sev ~ storrie_rdnbr + chips_rdnbr + slope , data=cover, na.rm=T)
#aspect and elevation were not signif
#overlays vectors onto rotated nmds
plot(sev.fit)
sev.fit$vectors

#add dominant species for each plot
dom <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/cdomin.csv")
text(z.sev, labels = dom$abun)

#add vectors for environmental variables


#ordicluster(ord=sev.fit, display = "sites", cluster=k, prune=3)
