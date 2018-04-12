#indicator species analysis of center sub-plot data  
library(tidyverse)
library(vegan)
library(indicspecies)
library(labdsv)

#data frame of response variables (all common species Relative cover values)
cover2 <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/coverRel.csv")
# dataframe of predictor variables (rdnbr, plot names & categories)
cover <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/cover1.csv")
#add dominant species for each plot
dom <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/cdomin.csv")

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

###############################################################
#creates NMDS ordination####
z <- metaMDS(comm = cover2, k=3, distance = "horn", weakties=T, trymax = 100, autotransform = FALSE)
z$stress

###########
#rotate NMDS by storrie severity####
z.sev <- MDSrotate(z, cover$storrie_rdnbr)

#plot
plot(z.sev, display = "sites")

#code points by dominant species
points(z.sev$points, col=dom$abun, pch=20, cex=2)
text(0,.55, "CEIN", cex=1.2, col="blue") 
text(.55,-.7, "CECO", cex=1.2, col="green4") 
text(-.6,-.8, "ABCO/QUKE", cex=1.2, col="black") 
title(main = "NMS with dominant species providing color coding")

#add continuous severity vectors
sev.fit <- envfit(z.sev ~ storrie_rdnbr + chips_rdnbr , data=cover, na.rm=T)
#aspect and elevation were not signif
#overlays vectors onto rotated nmds
plot(sev.fit)
sev.fit$vectors

# add circles around clusters of points  
library(plotrix)
draw.ellipse(-.1, .6, a=.5, b=.2, border = "blue", lwd=2)
draw.ellipse(.75, -.75, a=.5, b=.2, border = "green4", lwd=2)
draw.ellipse(-.8, -.85, a=.7, b=.2, border = "black", lwd=2)
#####################################################
#plot with ggplot

