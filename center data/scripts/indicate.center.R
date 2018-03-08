#indicator species analysis of center sub data  
library(tidyverse)

#data frame of response variables (ALL common species relative cover)
cover2 <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/importance.csv")
# dataframe of predictor variables (rdnbr, plot names & categories)
cover <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/ccover1.imp.csv")

#removes old categorical column
cover <- cover[,-2]

###creates Storrie and Chips categorical severity from plot # designations####
cover.sub <- cover %>% 
  separate(Plot, c("Storrie", "Chips", "plot"), remove = F)

cover.sub$Storrie [cover.sub$Storrie==1] <- "un"
cover.sub$Storrie [cover.sub$Storrie==2] <- "low" 
cover.sub$Storrie [cover.sub$Storrie==3] <- "mod"
cover.sub$Storrie [cover.sub$Storrie==4] <- "high"

cover.sub$Chips [cover.sub$Chips==1] <- "un"
cover.sub$Chips [cover.sub$Chips==2] <- "low" 
cover.sub$Chips [cover.sub$Chips==3] <- "mod"
cover.sub$Chips [cover.sub$Chips==4] <- "high"

#combines severities from each fire into one column
cover.sub <- cover.sub %>%
  unite(SC, Storrie, Chips, sep = "/")

#removes uncessary column "plot"
cover1 <- cover.sub[,-3]

#creates vector of storrie/chips severity combination
cat <- as.factor(cover1$SC)
##############################################################################################
#indicator species analysis using field-determined severity combinations####
library(indicspecies)
cov.ind <- multipatt(cover2, cluster = cat, control = how(nperm = 999), duleg = T)
cov.ind$sign
summary(cov.ind)

library(labdsv)
ind1 <- indval(cover2, cat)
ind1$indval
summary(ind1)
############################################################################################
#combine indicator species analysis with k means clustering####
k <- kmeans(cover2, centers = 4, nstart = 100)

ck <- cascadeKM(cover2, inf.gr = 2, sup.gr = 11) #evaluates # of groups
plot(ck) #indicates that 4 groups are best

#runs combined analysis 
kind <- multipatt(cover2, k$cluster, control = how(nperm = 999))
summary(kind, indvalcomp = TRUE)

library(cluster)
k4.pam <- pam(x=cover2, metric = "euclidean", k=4)

#creates NMDS ordination
library(vegan)
z <- metaMDS(comm = cover2, k=4, distance = "horn", weakties=T, trymax = 100, autotransform = FALSE)
plot(z, display = "sites")
points(z, pch=k$clustering, col=k4.pam$clustering, cex=2)


#rotate NMDS by storrie severity####
z.sev <- MDSrotate(z, cover$storrie_rdnbr)
plot(z.sev, display = "sites")
dom <- cover$dom
points(z.sev$points, col=k4.pam$clustering, cex=2) 

title(main = "Rotated with Storrie on X")

#overlays chips severity as vectors onto nmds
sev.fit <- envfit(z ~ storrie_rdnbr + chips_rdnbr, data=cover)
plot(z, display = "sites")
plot(sev.fit)

#############################################################################
#hierarchical clustering?#########
#creates dissimilarity matrix
cover2.dist <- vegdist(cover2, method = "horn")
#clusters
cover.hclust <- hclust(d=cover2.dist, method = "ward.D2")
plot(cover.hclust)
g4 <- cutree(cover.hclust, k=4)
#plots nmds
plot(z, display="sites")
ordicluster(ord=z, display = "sites", cluster=cover.hclust, prune=4)
ordihull(ord=z, groups = g4, col=c("blue", "green", "red", "orange"), lwd=2)
###########################################################################################
#creates categorical combined severity variable based on RdNBR values####
cover.cat <- mutate(cover, sto="", chip="")
cover.cat$sto [cover$storrie_rdnbr<641] <- "low"
cover.cat$sto [cover$storrie_rdnbr>=641] <- "high"

cover.cat$chip [cover$chips_rdnbr<641] <- "low"
cover.cat$chip [cover$chips_rdnbr>=641] <- "high"

cover.cat <- cover.cat %>%
  unite(cat2, sto, chip, sep="/")

cat2 <- cover.cat$cat2

#indicator species analysis using multipatt
library(indicspecies)
cov.ind2 <- multipatt(coverRel, cluster = cat2, control = how(nperm = 999))
cov.ind2$sign
summary(cov.ind2)

library(labdsv)
ind3 <- indval(coverRel, cat2)
ind3$indval
