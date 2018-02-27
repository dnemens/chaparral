#indicator species analysis of center sub data  
library(tidyverse)

#data frame of response variables (species relative cover)
coverRel <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/ccoverRel.csv")
# dataframe of predictor variables (rdnbr and plot names)
cover <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/ccover1.csv")

#creates categorical combined severity variable based on field severity determinations
cover.sub <- cover %>% 
  separate(Plot, c("Storrie", "Chips", "plot"), remove = F)

#replaces numbered categories with words (high=75%+ mortality)
cover.sub$Storrie [cover.sub$Storrie==1] <- "low"
cover.sub$Storrie [cover.sub$Storrie==2] <- "low" 
cover.sub$Storrie [cover.sub$Storrie==3] <- "low"
cover.sub$Storrie [cover.sub$Storrie==4] <- "high"

cover.sub$Chips [cover.sub$Chips==1] <- "low"
cover.sub$Chips [cover.sub$Chips==2] <- "low" 
cover.sub$Chips [cover.sub$Chips==3] <- "low"
cover.sub$Chips [cover.sub$Chips==4] <- "high"

#removes uneeded columns
cover.sub <- cover.sub[,1:3]

#combines severities from each fire into one column
cover.sub <- cover.sub %>%
  unite(cat, Storrie, Chips, sep = "/")

cat <- cover.sub$cat

#indicator species analysis?
library(indicspecies)
cov.ind <- multipatt(coverRel, cluster = cat, control = how(nperm = 999))
cov.ind$sign
summary(cov.ind)

library(labdsv)
ind1 <- indval(coverRel, cat)
ind1$indval
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
