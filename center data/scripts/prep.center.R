#preps center sub-plot data for analysis, creates data files of response data
library(tidyverse)
library(vegan)
library(labdsv)

#######################################################################
#creates a matrix of predictor variables####
##creates Storrie and Chips categorical severity from plot # designations
rdnbr <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/rdnbr.csv")

cover.sub <- rdnbr %>% 
  separate(Plot, c("Storrie", "Chips", "plot"), remove = F)

cover.sub$Storrie [cover.sub$Storrie==1] <- "un"
cover.sub$Storrie [cover.sub$Storrie==2] <- "burned" 
cover.sub$Storrie [cover.sub$Storrie==3] <- "burned"
cover.sub$Storrie [cover.sub$Storrie==4] <- "burned"

cover.sub$Chips [cover.sub$Chips==1] <- "un"
cover.sub$Chips [cover.sub$Chips==2] <- "burned" 
cover.sub$Chips [cover.sub$Chips==3] <- "burned"
cover.sub$Chips [cover.sub$Chips==4] <- "burned"

#combines severities from each fire into one column
cover.sub <- cover.sub %>%
  unite(SC, Storrie, Chips, sep = "/")

#removes uncessary column
cover1 <- cover.sub[,-3]

#saves data file of predictors, 16 combos, WITH 0 plots
write.csv(cover1, file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/cover1_93.csv", row.names = F)

#######################################################################################
#create a species-only response matrix####
cover.cent <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/center sub plot.csv", header = T)

#summarizes data sheet, giving total crown area per species for each plot
ccover.sum <- cover.cent %>%
  group_by(plot, Spp) %>%
  summarize(cover = sum(crown.area))

#transposes rows to columns
ccover <- spread(ccover.sum, key = "Spp", value = "cover", fill = 0.0)
#removes "plot" column
ccover <- ccover[,-1]

#removes rare species
cover2 <- vegtab(taxa = ccover, minval = (.05*nrow(ccover)))

#removes plots with no species present
Nozero <- data.frame(cover2, cover1)
Nozero2 <- Nozero[rowSums(Nozero[,1:12])!=0,]

cover2 <- Nozero2[,1:12]
cover1 <- Nozero2[,13:19]
#########################################################
#creates df of tree densities
den.cent <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/center sub plot.csv", header = T)

#summarizes data sheet, giving total crown area per species for each plot
cden.total <- den.cent %>%
  group_by(plot, Spp) %>%
  summarize(density = length(Spp))

#transposes rows to columns
cden <- spread(cden.total, key = "Spp", value = "density", fill = 0.0)
#removes "plot" column
cden <- cden[,-1]
#removes rare spp
cden2 <- vegtab(taxa = cden, minval = (.05*nrow(cden)))
#removes all but trees
cden2 <- cden2[,8:12]
cden2 <- cden2[,-3]
###########################################################
#create spreadsheets####

#saves data file of predictors, combos, without 0 plots
write.csv(cover1, file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/cover1.csv", row.names = F)

#saves data file of resulting reponse matrix, without 0 plots  -- 12 species left!####
write.csv(cover2, file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/coverRaw.csv", row.names = F)

#saves file of tree densities (93 plots!)
write.csv(cden2, file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/density.csv", row.names = F)
######################################################################
#other transformations: relative % cover, actual % cover

#calculates actual PERCENT cover for each species
coverAct <- cover2/5648
write.csv(coverAct, file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/coverAct.csv", row.names = F)

#relativizes by row totals - gives RELATIVE cover for each species
coverRel <- decostand(cover2, method = "total")
write.csv(coverRel, file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/coverRel.csv", row.names = F)


########################################################################################
#DOMINANT SPECIES####
#creates data frame with single response for each plot -- most common species in plot by relative cover
#finds species with highest cover for each plot, adds a column with that species' name
most.abundant2 <- mapply(function(y)
{a <- which(coverRel[y,] == max(coverRel[y,]), arr.ind=T)
names(coverRel[a[,2]])} , 1:length(coverRel[,1]))

#add an NA if more than one species is dominant
most.abundant3 <- mapply(function(y){
  if(length(most.abundant2[[y]]) > 1)
  {NA} else
  {most.abundant2[[y]]}
}, 1:length(most.abundant2))

#creates a new data frame with abundance codes for each plot, merged with rdnbr values
cover1 <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/cover1.csv")

cdomin <- data.frame(cover1, abun = most.abundant3)

#saves data frame as new spreadsheet
write.csv(cdomin, file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/cdomin.csv", row.names = F)

################################################################
