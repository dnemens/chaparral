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
cols <- rainbow(12)
palette(col.rainbow)
points(z.sev$points, bg=dom$abun, pch=21, cex=1.3)
text(.1,.5, "CEIN", cex=1.2, col="black") 
text(.5,-.7, "CECO", cex=1.2, col="black") 
text(-.5,-.75, "ABCO/QUKE", cex=1.2, col="black") 
title(main = "NMS with dominant species providing color coding")

#add continuous severity vectors
sev.fit <- envfit(z.sev ~ storrie_rdnbr + chips_rdnbr , data=cover, na.rm=T)
#aspect and elevation were not signif
#overlays vectors onto rotated nmds
plot(sev.fit)
sev.fit$vectors

# add circles around clusters of points  
library(plotrix)
draw.ellipse(-.1, .6, a=.5, b=.25, border = "black", lwd=2) #cein
draw.ellipse(.75, -.75, a=.5, b=.18, border = "black", lwd=2) #ceco
draw.ellipse(-.6, -.85, a=.7, b=.2, border = "black", lwd=2) #abco/quke
#####################################################
#plot with ggplot

