#preps data for analysis -- creates explan and response matrices, exploratory plot
#uses focal-oak subplot data

midstory <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/midstory.csv")

library(tidyverse)

#caluclates mean mid point for shrubs species by plot
cover <- midstory %>%
  group_by(Plot, spp) %>%
  summarize(cover = mean(shrub.cover.midpoint))

#removed na's (mostly trees)
cover <- na.omit(cover)

#reconfigures data such that each row is a plot, species are in columns, and cover values are in cells
cover <- spread(cover, key = "spp", value = "cover", fill = 0.0)
cover <- as.data.frame(cover)

###############################################################################################
#creates a species-only matrix (response)

#removes columns: plot and tree species
cover3 <- as.data.frame(cover [, -c(1,16, 21, 23)]) 
#removes rare species
library(labdsv)
cover2 <- vegtab(taxa = cover3, minval = (.05*nrow(cover3)))
#adds dummy species with cover=1 for distance meaures
cover2$fake <- 1

#creates data file of reponse matrix with most common species
write.csv(cover2, file="C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/cover2.csv", row.names = F)

##############################################################################################
#creates data frame with single response for each plot -- most common species in plot by cover
#finds species with highest cover for each plot, adds a column with that species' name
most.abundant2 <- mapply(function(y)
{a <- which(cover3[y,] == max(cover3[y,]), arr.ind=T)
names(cover3[a[,2]])} , 1:length(cover3[,1]))

#add an NA if more than one species is dominant
most.abundant3 <- mapply(function(y){
  if(length(most.abundant2[[y]]) > 1)
    {NA} else
    {most.abundant2[[y]]}
}, 1:length(most.abundant2))

#creates a new data frame with abundance codes for each plot, merges with rdnbr values
cover.s <- select_(cover, "Plot")
rdnbr <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/rdnbr.csv")
cover.comm <- merge(cover.s, rdnbr, by="Plot")

domin <- data.frame(cover.comm, abun = most.abundant3)

write.csv(domin, file="C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/domin.csv", row.names = F)

#########################################################################
#creates a matrix of predictor variables
##re-creates Storrie and Chips categorical severity columns
cover.sub <- cover.s %>% 
  separate(Plot, c("Storrie", "Chips", "plot"), remove = F)

cover.sub$Storrie [cover.sub$Storrie==1] <- "low"
cover.sub$Storrie [cover.sub$Storrie==2] <- "low" 
cover.sub$Storrie [cover.sub$Storrie==3] <- "low"
cover.sub$Storrie [cover.sub$Storrie==4] <- "high"

cover.sub$Chips [cover.sub$Chips==1] <- "low"
cover.sub$Chips [cover.sub$Chips==2] <- "low" 
cover.sub$Chips [cover.sub$Chips==3] <- "low"
cover.sub$Chips [cover.sub$Chips==4] <- "high"

cover.sub <- cover.sub[,1:3]

#combines severities from each fire into one column
cover.sub <- cover.sub %>%
  unite(SC, Storrie, Chips, sep = "/")

rdnbr <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/rdnbr.csv")
cover1 <- merge(cover.sub, rdnbr, by="Plot")

write.csv(cover1, file="C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/cover1.csv", row.names = F)

######################################################################
#exploratory plot
domin <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/domin.csv")

#vectors
sto <- domin$storrie_rdnbr
ch <-  domin$chips_rdnbr
cov <- factor(domin$abun)

#plots all plots on severity coordinates, labels each plot by dominant spp
plot (sto, ch, pch='', text(ch~sto, labels = cov, cex=.8), xlim=c(-500, 1100), xlab = "Storrie Fire severity", ylim=c(-500, 1100), ylab = "Chips Fire severity")
title(main="Focal oak midstory plot dominants by cover class")

#trying to plot this with ggplot!
library(ggplot2)
colors <- c("red", "green", "yellow", "orange", "blue", "purple", "pink", "black", "greenblue" )
#aaargh!
ggplot(domin, aes(x=sto, y=ch)) +
  geom_point(aes(x=sto, y=ch), colour = "grey") +
  geom_label(aes(sto), label = cov, fill=factor(cov))
  geom_text(x=sto, y=ch, label=cov, colour=factor(cov))
#bleeach!
  scale_fill_manual(values=c("red", "green", "yellow", "orange", "blue", "purple", "pink", "black", "greenblue" ))
  labs(y="Chips Fire severity", x="Storrie Fire severity") +
  theme(axis.title = element_text(size=20), axis.text = element_text(size=12, colour = "black"))+
  xlim(-500, 1100)+
  ylim(-500, 1100)
