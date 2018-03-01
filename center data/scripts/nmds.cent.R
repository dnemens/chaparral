#For NMDS using center sub plot data

library(vegan)

#loads response matrix - absolute cover for each spp
cover2 <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/ccoverAct.csv", header = T)

#loads explan matrix (rdnbr, severity combos)
cover1 <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/ccover1.csv", header = T)

#creates vector for severity combo category
cat <- cover1$SC

#loads dat frame with dominant species for each plot
domin <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/cdomin.csv", header = T)
dom <- domin$abun

#relativize cover data based on row max and plot total
#cover2.stand <- wisconsin(cover2)
#remove less common species and plots with no data
#cover2 <- cover2[,1:4]
#cover2 <- cover2[rowSums(cover2)!=0,]

#relativize cover data
#cover2.Rel <- decostand(cover2, method = "total")
#adds dummy species with cover=.01 for BC distance meaure
cover2$fake <- .01

#MDS
z <- metaMDS(comm = cover2, distance = "bray", k=2, weakties=T, trymax = 50)

stressplot(z, lwd=2)
z$stress

#plots the ordination
plot(z, display="sites")
#adds points, color-coded by dominant species
points(z, col = dom, pch=20, cex=1.5)

#overlays storrie and chips severity values onto ordination
ordi <- ordisurf(z, cover1$storrie_rdnbr, main="Burn severity")
#adds points, color-coded by dominant species
points(z, col = dom, pch=20, cex=1.5)


#extracts variables from ordisurf for plotting in ggplot
ordi.grid <- ordi$grid #extracts the ordisurf object
str(ordi.grid) #it's a list though - cannot be plotted as is
ordi.mite <- expand.grid(x = ordi.grid$x, y = ordi.grid$y) #get x and ys
ordi.mite$z <- as.vector(ordi.grid$z) #unravel the matrix for the z scores
ordi.mite.na <- data.frame(na.omit(ordi.mite)) #gets rid of the nas
ordi.mite.na #looks ready for plotting!

#rotates axes according to storrie severity (continuous)
z.sev <- MDSrotate(z, cover1$storrie_rdnbr)
plot(z.sev, display = "sites")
points(z.sev$points, col = cat, pch=16)

#or ggplot
gz <- data.frame(dom, scores(z), cover1$storrie_rdnbr, cover1$chips_rdnbr)
gz <- na.omit(gz)
  
library(ggplot2)
library(directlabels)
ggplot(gz, aes(x=NMDS1, y=NMDS2))  +
  geom_point(aes(shape=dom), size=3)+
  theme_classic()+
  stat_contour(data = ordi.mite.na, aes(x = x, y = y, z = z, colour = ..level..), cex=1.1)+
  scale_color_gradient(low="green", high="red")


setwd("/Users/dnemens/Dropbox/CBO/chaparral/plots")
ggsave(filename = "ggordi_storrie.jpg")
