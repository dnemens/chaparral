###creates data matrix based on importance values for shrub AND tree species####

library(tidyverse)
library(vegan)
library(labdsv)

##################################################################################
#creates a matrix of predictor variables
##creates Storrie and Chips categorical severity from plot # designations
rdnbr <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/rdnbr.csv")

cover.sub <- rdnbr %>% 
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

#removes uncessary column
cover1 <- cover.sub[,-3]

###################################################################################
#creates a species-only response matrix####
cover.cent <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/center sub plot.csv", header = T)

###COVER
#summarizes data sheet, giving total crown area per species for each plot
ccover.sum <- cover.cent %>%
  group_by(plot, Spp) %>%
  summarize(cover = sum(crown.area))

#transposes rows to columns
ccover <- spread(ccover.sum, key = "Spp", value = "cover", fill = 0.0)

#removes plot column
ccover.all <- as.data.frame(ccover [,-1])
#removes rare species
ccover.all <- vegtab(taxa=ccover.all, minval = (.05*nrow(ccover)))
#relativizes by row totals - gives RELATIVE cover for each species
ccover.allR <- decostand(ccover.all, method = "total")

#DENSITY
##summarizes data sheet, giving density of each species by plot
density.sum <- cover.cent %>%
  group_by(plot, Spp) %>%
  summarize(freq = length(Spp))
#transposes rows to columns
density <- spread(density.sum, key = "Spp", value = "freq", fill = 0.0)

#removes plot column
density <- as.data.frame(density [,-1])
#removes rare species
density <- vegtab(taxa=ccover.all, minval = (.05*nrow(ccover)))
#relativizes by row totals
densityR <- decostand(ccover.all, method = "total")

###Calculate importance values for each species for each plot####
imp <- data.frame((ccover.allR+densityR)*100)
imp <- round(imp, 0)
#################################################################################
#creates data frame with single response for each plot -- most common species in plot by cover
#finds species with highest cover for each plot, adds a column with that species' name
most.abundant2 <- mapply(function(y)
{a <- which(imp[y,] == max(imp[y,]), arr.ind=T)
names(imp[a[,2]])} , 1:length(imp[,1]))

#add an NA if more than one species is dominant
most.abundant3 <- mapply(function(y){
  if(length(most.abundant2[[y]]) > 1)
  {NA} else
  {most.abundant2[[y]]}
}, 1:length(most.abundant2))
##################################################################################
#removes blank rows!!  
Nozero <- data.frame(imp, cover1, dom=most.abundant3)
Nozero <- Nozero[rowSums(Nozero[,1:12])!=0,]

imp.no0 <- Nozero[,1:12]
ccover1.no0 <- Nozero[,13:17]

####creates csv's from 0-removed data frames
write.csv(imp.no0, file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/importance.csv", row.names = F)
write.csv(ccover1.no0, file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/ccover1.imp.csv", row.names = F)

