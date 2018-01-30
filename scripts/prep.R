midstory <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/midstory.csv")

library(tidyverse)

#caluclates mean mid point for shrubs species by plot
cover <- midstory %>%
  group_by(Plot, spp) %>%
  summarize(cover = mean(shrub.cover.midpoint))

#removed na's (mostly trees)
cover <- na.omit(cover)

#reconfigures data such that each row is a plot, species are in columns, and cover values are in cells
cover <- spread(cover, key = "spp", value = "cover", fill = 0)


#density.c <- center %>%
 # filter(Spp == "ABCO" | Spp == "PSME") %>%
  #filter(ht>=3) %>%
 # group_by(plot) %>%
#  summarize(abco.density = length(which(Spp == "ABCO")), psme.density = length(which(Spp == "PSME")))

#density.c <- mutate(density.c, den.ha.abco = density.c$abco.density*177, den.ha.psme = psme.density*177)