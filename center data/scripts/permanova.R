#PERMANOVA of cover data
library(tidyverse)
library(vegan)
library(devtools)

#data frame of response variables (ALL common species Relative cover values)
cover2 <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/coverRel.csv")

# dataframe of predictor variables (rdnbr, plot names & categories)
cover <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/cover1.csv")

#B-C distance matrix
cov.dist <- vegdist(cover2, method = "bray")

#conduct a kmeans cluster analysis
k <- kmeans(cover2, centers = 4, nstart = 100)

#PERMANOVA
#simple test for overall effect of cluster
try1 <- adonis(cover2~k$cluster, data = cover2)
try1


#test for overall effect of severity combo####
try2 <- adonis(cover2~cover$SC)
try2  


#multiple pairwise comparisions
x <- betadisper(cov.dist, cover$SC)
anova(x)
tx <- TukeyHSD(x)
plot(tx)
tx

#multiple comparisons?
cat <- cover$SC
library(pairwiseAdonis)
pairwise.adonis(cover2, cat)

#test for effect of continuous severity measurement on species comp####
sto <- cover$storrie_rdnbr
chip <- cover$chips_rdnbr
try3 <- adonis(cover2~sto*chip)
try3

try4 <- adonis(cover2~sto+sto%in%chip)
try4
