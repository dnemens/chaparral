#indicator species analysis of center sub data  
library(tidyverse)

#data frame of response variables (ALL common species importance values)
cover2 <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/importance.csv")
# dataframe of predictor variables (rdnbr, plot names & categories)
cover <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/cover1.csv")

#creates vector of storrie/chips severity combination
cat <- as.factor(cover$SC)
##############################################################################################
#indicator species analysis using field-determined severity combinations####
library(indicspecies)
cov.ind <- multipatt(cover2, cluster = cat, control = how(nperm = 999), duleg = T)
cov.ind$sign
summary(cov.ind)

#extracts indicator value for each significant species
library(labdsv)
ind1 <- indval(cover2, cat)
ind1$indval
summary(ind1)
############################################################################################
#combine indicator species analysis with k means clustering####
#choose # of groups
ck <- cascadeKM(cover2, inf.gr = 2, sup.gr = 12) #evaluates # of groups 
plot(ck) #indicates that 4 groups are best?

#conduct a kmeans cluster analysis
k <- kmeans(cover2, centers = 4, nstart = 100)

#runs combined analysis 
#indicator species analysis with kmeans cluster determining groups
kind <- multipatt(cover2, k$cluster, control = how(nperm = 999))
summary(kind, indvalcomp = TRUE)

#should I use the pam function, since the distance matrix above may not be appropriate?  
library(cluster)
k4.pam <- pam(x=cover2, metric = "euclidean", k=4)
summary(k4.pam)
###############################################################
#creates NMDS ordination####
library(vegan)
z <- metaMDS(comm = cover2, k=4, distance = "horn", weakties=T, trymax = 100, autotransform = FALSE)
z$stress
plot(z, display = "sites")
points(z, pch=20, col=k$cluster, cex=2)
ordihull(z, groups = k$cluster, col=k$cluster)

################################################################
#rotate NMDS by storrie severity####
z.sev <- MDSrotate(z, cover$storrie_rdnbr)
plot(z.sev, display = "sites")
#code points by cluster from kmeans
points(z.sev$points, col=k$cluster, pch=20, cex=2) 
ordihull(z.sev, groups = k$cluster, col=c("black", "red", "green", "blue"))
title(main = "Rotated with Storrie on X")

#add continuous severity vectors
sev.fit <- envfit(z.sev ~ storrie_rdnbr + chips_rdnbr, data=cover)
#overlays chips severity as vectors onto rotated nmds
plot(sev.fit)


ordicluster(ord=sev.fit, display = "sites", cluster=k, prune=3)
