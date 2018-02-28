#indicator species analysis of center sub data  
library(tidyverse)

#data frame of response variables (species relative cover)
coverRel <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/ccoverRel.csv")
# dataframe of predictor variables (rdnbr, plot names & categories)
cover <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/ccover1.csv")

cat <- cover$SC
##############################################################################################

#indicator species analysis?
library(indicspecies)
cov.ind <- multipatt(coverRel, cluster = cat, control = how(nperm = 999))
cov.ind$sign
summary(cov.ind)

library(labdsv)
ind1 <- indval(coverRel, cat)
ind1$indval
summary(ind1)
############################################################################################
#creates categorical combined severity variable based on RdNBR values
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
