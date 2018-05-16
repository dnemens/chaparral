#indicator species analysis of center sub-plot data  
library(tidyverse)
library(vegan)
library(indicspecies)
library(labdsv)

#data frame of response variables (all common species Relative cover values) = coverRel.csv
cover2 <- read.table(file.choose(), T)

# dataframe of predictor variables (rdnbr, plot names & categories) = cover1.csv
cover <- read.table(file.choose(), T)
# dominant species for each plot = cdomin.csv
dom <- read.table(file.choose(), T)

#creates vector of storrie/chips severity combination
cat <- as.factor(cover$SC)
##############################################################################################
#indicator species analysis using field-determined severity combinations####
cov.ind <- multipatt(cover2, cluster = cat, control = how(nperm = 999), duleg = T)
summary(cov.ind, indvalcomp = T)

#extracts indicator value for each significant species
ind1 <- indval(cover2, cat)
ind1$indval
summary(ind1, alpha=1)

############################################################################################
#combine indicator species analysis with k means clustering####??

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
#this yields very high indicator values!
###############################################################
#Ordination####

#creates NMDS ordination####
z <- metaMDS(comm = cover2, k=3, distance = "horn", weakties=T, trymax = 200, autotransform = FALSE)
z$stress #0.079


#Plots the ordination###############################
#rotate NMDS by storrie severity####
z.sev <- MDSrotate(z, cover$storrie_rdnbr)
plot(z.sev, display = "sites")

#code points by cluster from kmeans
#points(z.sev$points, col=k$cluster, pch=20, cex=2)

#adds species code dominant species for each plot - this is just to check on which factor is causing the clustering, I'll find a more elegant way to present this in the final plot
text(z.sev, labels=k$cluster, cex=1.2, col="black") 
ordihull(z.sev, groups = k$cluster, col=c("black", "red", "green", "blue"))
title(main = "Numbers from cluster identity")

#add continuous severity vectors
sev.fit <- envfit(z.sev ~ storrie_rdnbr + chips_rdnbr + aspect + elev + slope, data=cover, na.rm=T)

#overlays vectors onto rotated nmds
plot(sev.fit)
sev.fit$vectors

