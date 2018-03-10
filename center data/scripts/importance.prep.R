###creates data matrix based on importance values for shrub AND tree species####

library(tidyverse)
library(vegan)
library(labdsv)

###############################################################
##loads severity data with all 93 plots included
cover1 <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/cover1_93.csv")

###################################################################################
#creates a species-only response matrix of importance values####
cover.cent <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/center sub plot.csv", header = T)

###COVER
#summarizes data sheet, giving total crown area per species for each plot
cover.sum <- cover.cent %>%
  group_by(plot, Spp) %>%
  summarize(cover = sum(crown.area))

#transposes rows to columns
covert <- spread(cover.sum, key = "Spp", value = "cover", fill = 0.0)

#removes plot column
cover.all <- as.data.frame(covert [,-1])
#removes rare species
cover.all <- vegtab(taxa=cover.all, minval = (.05*nrow(cover.all)))
#relativizes by row totals - gives RELATIVE cover for each species
cover.allR <- decostand(cover.all, method = "total")

#calculates DENSITY 
##summarizes data sheet, giving density of each species by plot
density.sum <- cover.cent %>%
  group_by(plot, Spp) %>%
  summarize(dens = length(Spp))
#transposes rows to columns
density <- spread(density.sum, key = "Spp", value = "dens", fill = 0.0)

#removes plot, forb and grams column
density <- density[,-1]
#removes rare species
density <- vegtab(taxa=density, minval = (.05*nrow(density)))
#removes forb and gram columns
density <- density[,-c(1,2,15)]
#relativizes by row totals
densityR <- decostand(density, method = "total")

###Calculate importance values for each species for each plot####
imp <- data.frame((cover.allR+densityR)*100)
imp <- round(imp, 0)

##################################################################################
#removes blank rows!!  
Nozero <- data.frame(imp, cover1)
Nozero <- Nozero[rowSums(Nozero[,1:12])!=0,]

imp.no0 <- Nozero[,1:12]

####creates csv from 0-removed importance value data matrix
write.csv(imp.no0, file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/importance.csv", row.names = F)

