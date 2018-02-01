#for nmds using midstory

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

cover2 <- as.data.frame(cover [, -c(1,16, 21, 23)]) #creates a species-only matrix (response)

#removes rare species
library(labdsv)
cover2 <- vegtab(taxa = cover2, minval = (.05*nrow(cover2)))
#adds dummy species with cover=1 for distance meaure
cover2$fake <- 1

#NMDS
library(vegan)
z <- metaMDS(comm = cover2, distance = "bray", k=2, weakties=T)

stressplot(z, lwd=2)

#plots nmds
plot(z, type="n")
points(z, display = "")

#overlays dominant species name onto plot
text(domin, labels=cov)

st <- cover.sub$Storrie
ch <- cover.sub$Chips

#overlays severity values onto plot
text(z, labels=st, col="red")
text(z, labels=ch, col="green", pos=4)



#uses severity to rotate plot
MDSrotate()
#########################################################################
#creates a matrix of predictor variables
#retains only plot name
cover.sub <- as.data.frame(cover[,1])
##re-creates Storrie and Chips categorical severity columns
cover.sub <- cover.sub %>% 
  separate(Plot, c("Storrie", "Chips", "plot"), remove = F) %>% 
  mutate(1=="un", 2=="low", 3=="mod", 4=="high")

rdnbr <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/rdnbr.csv")
cover1 <- merge(cover.sub, rdnbr, by="Plot")


cover1 <- cover1 %>% 
  seperate(Plot, c("Storrie", "Chips", "plot"))
  





library(labdsv)

