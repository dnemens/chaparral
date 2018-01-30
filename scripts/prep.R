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

cover.comm <- cover %>%
  select(ARPA, CECO, CEIN, RIRO, CEVE, SYMO)
 
cover.comm.sub <- cover.comm [,-c(1)]

#would use this to remove rare species if I could get it to work!! 
#Plot col needs to be removed
#library(labdsv)
#vegtab(taxa = cover, minval = (nrow(cover==10)))
