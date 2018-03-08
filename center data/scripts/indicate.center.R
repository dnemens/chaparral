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
#indicator species analysis?
library(indicspecies)
cov.ind <- multipatt(cover2, cluster = cat, control = how(nperm = 999), duleg = T)
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
