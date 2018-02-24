#exploratory plots

#loads dataframe of dominant shrub species for each plot (NA's where two spp codom)
cdomin <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/cdomin.csv")

#vectors
sto <- cdomin$storrie_rdnbr
ch <-  cdomin$chips_rdnbr
cov <- factor(cdomin$abun)

#plots all plots on severity coordinates, labels each plot by dominant spp
plot (sto, ch, pch='', text(ch~sto, labels = cov, cex=.8), xlim=c(-500, 1100), xlab = "Storrie Fire severity", ylim=c(-500, 1100), ylab = "Chips Fire severity")
title(main="Center sub-plot dominants by crown area")

#same plot in ggplot, with points color-coded by dominant species
library(ggplot2)
ggplot(cdomin, aes(sto, ch)) + 
  theme_classic()+
  theme (panel.border = element_rect(fill = NA))+
  geom_point (aes(y=ch, color=cov), size=4)


#################################################################################
#data frame of response variables (species relative cover)
cover <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/ccover2.csv")
plot <- cdomin$plot
cover <- mutate(cover, plot = plot)

#merges rdnbr values with relative cover
rdnbr <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/rdnbr.csv")
rdnbr <- rdnbr %>% rename(plot=Plot)
cover <- merge(cover, rdnbr, by = "plot")

cein <- cover$CEIN

