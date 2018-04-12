#PERMANOVA of cover data
library(tidyverse)
library(vegan)
library(devtools)

#data frame of response variables (ALL common species Relative cover values)
cover2 <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/coverRel.csv")

# dataframe of predictor variables (rdnbr, plot names & categories)
cover <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/cover1.csv")


#test for overall effect of severity combo####
try2 <- adonis(cover2~cover$SC)
try2  


#multiple comparisons?
cat <- cover$SC
library(pairwiseAdonis)
pairwise.adonis(cover2, cat)

#or this for multiple pairwise comparisions
x <- betadisper(cov.dist, cover$SC)
anova(x)
tx <- TukeyHSD(x)
plot(tx)
tx

#test for effect of continuous severity measurement on species comp####
sto <- cover$storrie_rdnbr
chip <- cover$chips_rdnbr
try3 <- adonis(cover2~sto*chip)
try3

#interaction effects (nested factors)
try4 <- adonis(cover2~chip+sto%in%chip)
try4

#how to plot interaction?  

