#preps center sub-plot data for analysis, creates data files of response data
library(tidyverse)

cover.cent <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/center sub plot.csv", header = T)

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

#relativizes by row totals - gives RELATIVE cover for each species
library(vegan)
ccoverRel <- decostand(ccover2, method = "total")

#adds dummy species with cover=1 for distance meaures
#ccover2$fake <- 1

#creates data file of reponse matrix with most common species >5% frequency
write.csv(ccoverRel, file="C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/ccoverRel.csv", row.names = F)

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
rdnbr <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/rdnbr.csv")

cdomin <- data.frame(rdnbr, abun = most.abundant3)

#saves data frame as new spreadsheet
write.csv(cdomin, file="C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/cdomin.csv", row.names = F)
#######################################################################
