#preps center sub-plot data for analysis

cover.cent <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/center sub plot.csv", header = T)

library(tidyverse)

#summarizes data sheet, giving mean crown area by species
cover.c.sum <- cover.cent %>%
  group_by(plot, Spp) %>%
  summarize(cover = mean(crown.area))

ccover <- spread(cover.c.sum, key = "Spp", value = "cover", fill = 0.0)

#creates a species-only matrix (response)

#removes columns: plot, trees and v1
ccover3 <- as.data.frame(ccover [, -c(1,2, 3, 7, 25:32)]) 
#removes rare species
library(labdsv)
ccover2 <- vegtab(taxa = ccover3, minval = (.05*nrow(ccover3)))

#adds dummy species with cover=1 for distance meaures
#ccover2$fake <- 1

#creates data file of reponse matrix with most common species
#write.csv(ccover2, file="C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/ccover2.csv", row.names = F

#creates data frame with single response for each plot -- most common species in plot by cover
#finds species with highest cover for each plot, adds a column with that species' name
most.abundant2 <- mapply(function(y)
{a <- which(ccover2[y,] == max(ccover2[y,]), arr.ind=T)
names(ccover2[a[,2]])} , 1:length(ccover2[,1]))

#add an NA if more than one species is dominant
most.abundant3 <- mapply(function(y){
  if(length(most.abundant2[[y]]) > 1)
  {NA} else
  {most.abundant2[[y]]}
}, 1:length(most.abundant2))

#creates a new data frame with abundance codes for each plot, merged with rdnbr values
ccover.s <- select_(ccover, "plot")
rdnbr <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/rdnbr.csv")
rdnbr <- rdnbr %>% rename(plot=Plot)
ccover.s <- merge(ccover.s, rdnbr, by="plot")

cdomin <- data.frame(ccover.s, abun = most.abundant3)

#saves data frame as new spreadsheet
write.csv(cdomin, file="C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/cdomin.csv", row.names = F)
#######################################################################
#exploratory plot

#vectors
sto <- cdomin$storrie_rdnbr
ch <-  cdomin$chips_rdnbr
cov <- factor(cdomin$abun)

#plots all plots on severity coordinates, labels each plot by dominant spp
plot (sto, ch, pch='', text(ch~sto, labels = cov, cex=.8), xlim=c(-500, 1100), xlab = "Storrie Fire severity", ylim=c(-500, 1100), ylab = "Chips Fire severity")
title(main="Center sub-plot dominants by crown area")
