#For NMDS using center sub plot data

library(vegan)

#loads response matrix - IMPORTANCE values for each spp
cover2 <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/importance.csv", header = T)

#loads explan matrix (rdnbr, severity combos)
cover <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/ccover1.imp.csv", header = T)

#creates vector forseverity combo category
cat <- cover$SC
#vector for most important species in each plot
dom <- as.factor(cover$dom)

#loads dat frame with dominant species for each plot 
####would need to redo domin for 91 plots
#domin <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/cdomin.no0.csv", header = T)
#dom <- domin$abun

#relativize cover data based on row max and plot total
#cover2.stand <- wisconsin(cover2)
#remove less common species and plots with no data
#cover2 <- cover2[,1:4]

#adds dummy species with cover=.01 for BC distance meaure
#cover2$fake <- .01

#MDS
z <- metaMDS(comm = cover2, k=3, distance = "horn", weakties=T, trymax = 100, autotransform = FALSE)

stressplot(z, lwd=2) #Shepard plot
z$stress

#plots the ordination
plot(z, display="sites")
#adds points, color-coded by dominant species
points(z, col = dom, pch=20, cex=1.5)
legend("topright", levels(dom), fill=dom)

##overlays severity values onto ordination####

#storrie severity
ordiSto <- ordisurf(z, cover$storrie_rdnbr, main="Burn severity")
#extracts variables from ordisurf for plotting in ggplot
Sordi.grid <- ordiSto$grid #extracts the ordisurf object
Sordi.mite <- expand.grid(x = Sordi.grid$x, y = Sordi.grid$y) #get x and ys
Sordi.mite$z <- as.vector(Sordi.grid$z) #unravel the matrix for the z scores
Sordi.mite.na <- data.frame(na.omit(Sordi.mite)) #gets rid of the nas

#chips severity 
ordiChip <- ordisurf(z, cover$chips_rdnbr, main="Burn severity")
#extracts variables from ordisurf for plotting in ggplot
Cordi.grid <- ordiChip$grid #extracts the ordisurf object
Cordi.mite <- expand.grid(x = Cordi.grid$x, y = Cordi.grid$y) #get x and ys
Cordi.mite$z <- as.vector(Cordi.grid$z) #unravel the matrix for the z scores
Cordi.mite.na <- data.frame(na.omit(Cordi.mite)) #gets rid of the nas


#or ggplot
gz <- data.frame(dom, scores(z), cover$storrie_rdnbr, cover$chips_rdnbr)
  
library(ggplot2)
library(directlabels)
Sto <- ggplot(gz, aes(x=NMDS1, y=NMDS2))  +
  geom_point()
  geom_point(aes(shape=dom), size=3)+
  theme_classic()+
  stat_contour(data = Sordi.mite.na, aes(x = x, y = y, z = z, colour = ..level..), cex=1.1)+
  scale_color_gradient(low = "green", high ="red")+
  labs(colour="Fire severity\n(RdNBR)", title="Storrie Fire, r2=0.24")+
  theme(plot.title = element_text(hjust=.5, size=15))

Chip <- ggplot(gz, aes(x=NMDS1, y=NMDS2))  +
  geom_point(aes(shape=dom), size=3)+
  theme_classic()+
  stat_contour(data = Cordi.mite.na, aes(x = x, y = y, z = z, colour = ..level..), cex=1.1)+
  scale_color_gradient(low = "green", high ="red")+
  labs(colour="Fire severity\n(RdNBR)", title="Chips Fire, r2=0.29")+
  theme(plot.title = element_text(hjust=.5, size=15))

library(grid)
library(gridExtra)

#creates uniform widths for plots
#Sto <- ggplot_gtable(ggplot_build(Sto))
maxWidth = unit.pmax(Chip$widths[5:3], Sto$widths[5:3])
Chip$widths[5:3] <- maxWidth
Sto$widths[5:3] <- maxWidth

#stack both ggplots
both <- grid.arrange(Sto, Chip, nrow=1, ncol=2)

setwd("/Users/dnemens/Dropbox/CBO/chaparral/plots")
ggsave(fine, filename = "ggordi_sidebyside.jpg")

##################################################################
#rotates axes according to storrie severity (continuous)
z.sev <- MDSrotate(z, cover1$storrie_rdnbr)
plot(z.sev, display = "sites")
points(z.sev$points, col = cat, pch=16)

