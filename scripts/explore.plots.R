#exploratory plots

#loads dataframe of dominant shrub species for each plot (NA's where two spp codom)
cdomin <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/data sheets/cdomin.csv")

#vectors
sto <- cdomin$storrie_rdnbr
ch <-  cdomin$chips_rdnbr
cov <- factor(cdomin$abun)

#plots all plots on severity coordinates, labels each plot by dominant spp
plot (sto, ch, pch='', text(ch~sto, labels = cov, cex=.8), xlim=c(-500, 1300), xlab = "Storrie Fire severity", ylim=c(-500, 1300), ylab = "Chips Fire severity")
title(main="Center sub-plot dominants by crown area")

#same plot in ggplot, with points color-coded by dominant species
library(ggplot2)
ggplot(cdomin, aes(sto, ch)) + 
  theme_classic()+
  theme (panel.border = element_rect(fill = NA))+
  xlim(-500, 1220)+
  ylim(-500, 1220)+
  geom_point (aes(y=ch, color=cov, label=cov), size=3)


#################################################################################


