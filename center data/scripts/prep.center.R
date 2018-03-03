#preps center sub-plot data for analysis, creates data files of response data
library(tidyverse)

#######################################################################
#creates a matrix of predictor variables
##creates Storrie and Chips categorical severity from plot # designations
rdnbr <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/rdnbr.csv")

cover.sub <- rdnbr %>% 
  separate(Plot, c("Storrie", "Chips", "plot"), remove = F)

cover.sub$Storrie [cover.sub$Storrie==1] <- "low"
cover.sub$Storrie [cover.sub$Storrie==2] <- "low" 
cover.sub$Storrie [cover.sub$Storrie==3] <- "low"
cover.sub$Storrie [cover.sub$Storrie==4] <- "high"

cover.sub$Chips [cover.sub$Chips==1] <- "low"
cover.sub$Chips [cover.sub$Chips==2] <- "low" 
cover.sub$Chips [cover.sub$Chips==3] <- "low"
cover.sub$Chips [cover.sub$Chips==4] <- "high"

#combines severities from each fire into one column
cover.sub <- cover.sub %>%
  unite(SC, Storrie, Chips, sep = "/")

#removes uncessary column
cover1 <- cover.sub[,-3]

write.csv(cover1, file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/ccover1.csv", row.names = F)
#######################################################################################

cover.cent <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/center sub plot.csv", header = T)

#summarizes data sheet, giving total crown area per species for each plot
ccover.sum <- cover.cent %>%
  group_by(plot, Spp) %>%
  summarize(cover = sum(crown.area))

ccover <- spread(ccover.sum, key = "Spp", value = "cover", fill = 0.0)

#creates a species-only matrix (response)

#removes columns: plot, trees and v1
ccover1 <- as.data.frame(ccover [, -c(1,2, 3, 7, 25:32)]) 
#removes rare species
library(labdsv)
ccover2 <- vegtab(taxa = ccover1, minval = (.05*nrow(ccover1)))

#creates data file of reponse matrix with most common species >5% frequency
write.csv(ccover2, file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/ccoverRaw.csv", row.names = F)

#calculates actual PERCENT cover for each species
ccoverAct <- ccover2/5648
write.csv(ccoverAct, file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/ccoverAct.csv", row.names = F)

#relativizes by row totals - gives RELATIVE cover for each species
library(vegan)
ccoverRel <- decostand(ccover2, method = "total")

#creates data file of RELATIVE COVER reponse matrix with most common species >5% frequency
write.csv(ccoverRel, file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/ccoverRel.csv", row.names = F)

#########################################################
#removes blank rows!!  
Nozero <- data.frame(ccoverAct, cover1)
Nozero2 <- Nozero[rowSums(Nozero[,1:7])!=0,]

ccoverAct.no0 <- Nozero2[,1:7]
ccover1.no0 <- Nozero2[,8:11]

write.csv(ccoverAct.no0, file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/ccoverAct.no0.csv", row.names = F)
write.csv(ccover1.no0, file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/ccover1.no0.csv", row.names = F)


########################################################################################
#creates data frame with single response for each plot -- most common species in plot by cover
#finds species with highest cover for each plot, adds a column with that species' name
most.abundant2 <- mapply(function(y)
{a <- which(ccoverRel[y,] == max(ccoverRel[y,]), arr.ind=T)
names(ccoverRel[a[,2]])} , 1:length(ccoverRel[,1]))

#add an NA if more than one species is dominant
most.abundant3 <- mapply(function(y){
  if(length(most.abundant2[[y]]) > 1)
  {NA} else
  {most.abundant2[[y]]}
}, 1:length(most.abundant2))

#creates a new data frame with abundance codes for each plot, merged with rdnbr values
rdnbr <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/rdnbr.csv")

cdomin <- data.frame(rdnbr, abun = most.abundant3)

#saves data frame as new spreadsheet
write.csv(cdomin, file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/cdomin.csv", row.names = F)

################################################################
#same as above, removing plots with 0's

#creates data frame with single response for each plot -- most common species in plot by cover
#finds species with highest cover for each plot, adds a column with that species' name
most.abundant2 <- mapply(function(y)
{a <- which(ccoverRel.no0[y,] == max(ccoverRel.no0[y,]), arr.ind=T)
names(ccoverRel.no0[a[,2]])} , 1:length(ccoverRel.no0[,1]))

#add an NA if more than one species is dominant
most.abundant3 <- mapply(function(y){
  if(length(most.abundant2[[y]]) > 1)
  {NA} else
  {most.abundant2[[y]]}
}, 1:length(most.abundant2))

cdomin <- data.frame(cover1.no0, abun = most.abundant3)

#saves data frame as new spreadsheet
write.csv(cdomin, file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/cdomin.no0.csv", row.names = F)

