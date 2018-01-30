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
cover2 <- as.data.frame(cover [, -c(1,16, 21, 23)]) #removes columns: plot and tree species

#finds species with highest cover for each plot, adds a column with that species' name
most.abundant2 <- mapply(function(y)
  
{a <- which(cover2[y,] == max(cover2[y,]), arr.ind=T)

names(cover2[a[,2]])} , 1:length(cover2[,1]))

#add an NA if more than one species is dominant
most.abundant3 <- mapply(function(y){
  if(length(most.abundant2[[y]]) > 1)
    {NA} else
    {most.abundant2[[y]]}
}, 1:length(most.abundant2))

#creates a new data frame with abundance codes for each plot, merges with rdnbr values
cover.sub <- select(cover, Plot)
rdnbr <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/rdnbr.csv")
cover.comm <- merge(cover.sub, rdnbr, by="Plot")

domin <- data.frame(cover.comm, abun = most.abundant3)

write.csv(domin, file="C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/domin.csv")

sto <- domin$storrie_rdnbr
ch <-  domin$chips_rdnbr
cov <- domin$abun

#plots all plots on severity coordinates, labels each plot by dominant spp
plot (sto, ch, pch='', text(ch~sto, labels = cov), xlim=c(-500, 1100), xlab = "Storrie Fire severity", ylim=c(-500, 1100), ylab = "Chips Fire severity")

library(ggplot2)
ggplot(domin, aes(x=sto, y=ch)) +
  geom_point(aes(x=sto, y=ch)) +
  geom_label(data=cov)

  


#would use this to remove rare species if I could get it to work!! 
#Plot col needs to be removed first
#library(labdsv)
#vegtab(taxa = cover, minval = (nrow(cover==10)))
