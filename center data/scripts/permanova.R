#PERMANOVA of cover data
library(tidyverse)
library(vegan)
library(devtools)

#data frame of response variables (ALL common species Relative cover values)
cover2 <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/coverRel.csv")
# dataframe of predictor variables (rdnbr, plot names & categories)
cover <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/cover1.csv")

#creates vector of storrie/chips severity combination
cat <- as.factor(cover$SC)

#B-C distance matrix
cov.dist <- vegdist(cover2, method = "bray")

#PERMANOVA
#simple test for overall effect
try1 <- adonis(cover2~cat, data = cover2)
try1

#multiple comparisons?
library(pairwiseAdonis)
pairwise.adonis(cover2, cat, reduce = "high|low")

#create model matrix for comparisons of severity combos
cat.mm <- model.matrix(~C(cat, contr.sum(1,1,-1)))

#or this?
x <- betadisper(cov.dist, cat)
anova(x)
tx <- TukeyHSD(x)
plot(tx)
