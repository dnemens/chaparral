#indicator species analysis of center sub-plot data  
library(tidyverse)
library(vegan)
library(indicspecies)
library(labdsv)
library(ggrepel)

#data frame of response variables (all common species Relative cover values)
cover2 <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/coverRel.csv")
# dataframe of predictor variables (rdnbr, plot names & categories)
cover <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/cover1.csv")
#add dominant species for each plot
dom <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/cdomin.csv")

#creates vector of storrie/chips severity combination
cat <- as.factor(cover$SC)
domin <- dom$abun
##############################################################################################
#indicator species analysis using field-determined severity combinations####
cov.ind <- multipatt(cover2, cluster = cat, control = how(nperm = 99), duleg = F)
summary(cov.ind, indvalcomp = T)

#extracts indicator value for each significant species
ind1 <- indval(cover2, cat)
summary(ind1, alpha=1)

###############################################################
#creates NMDS ordination####
z <- metaMDS(comm = cover2, k=3, distance = "bray", weakties=T, trymax = 150, autotransform = FALSE)
z$stress
stressplot(z)

###########
#rotate NMDS by storrie severity####
z.sev <- MDSrotate(z, cover$storrie_rdnbr)

#plot
plot(z.sev, display = "sites")

cols <- rainbow(12)
palette(cols)

points(z.sev, col=cat, pch=20, cex=4)
text(z.sev$points, labels=domin, col="black", cex=.9)
legend('topright', legend = legs, col=cols)

legs <- c("un/un", "un/low", "low/un", "low/low", "un/high", "high/un", "low/high", "high/low","high/high")
#code points by dominant species
#points(z.sev$points, bg=dom$abun, pch=21, cex=1.3)


text(.1,.5, "CEIN", cex=1.2, col="black") 
text(.5,-.7, "CECO", cex=1.2, col="black") 
text(-.5,-.75, "ABCO/QUKE", cex=1.2, col="black") 
title(main = "NMS with burn serverity combination providing color coding")

#add continuous severity vectors

#PCA of rdnbr values
df <- data.frame(cover$storrie_rdnbr, cover$chips_rdnbr)
pc.rdnbr <- pca(df)

PC1 <- pc.rdnbr$scores[,1]
PC2 <- pc.rdnbr$scores[,2]
Storrie <- cover$storrie_rdnbr
Chips <- cover$chips_rdnbr

sev.fit <- envfit(z.sev ~ Storrie+Chips+PC1+PC2, data=cover, na.rm=T)
#aspect and elevation were not signif

#overlays vectors onto rotated nmds
plot(sev.fit)
sev.fit$vectors



# add circles around clusters of points  
library(plotrix)
draw.ellipse(-.1, .6, a=.5, b=.25, border = "black", lwd=2) #cein
draw.ellipse(.75, -.75, a=.5, b=.18, border = "black", lwd=2) #ceco
draw.ellipse(-.6, -.85, a=.7, b=.2, border = "black", lwd=2) #abco/quke
#####################################################
#same in ggplot!
scores <- as.data.frame(scores(z.sev, "sites"))
#scores$species <- rownames(scores)
#pal = c('#a50026','#d73027','#f46d43','#fdae61','#fee08b','#ffffbf','#d9ef8b','#a6d96a','#66bd63','#1a9850','#006837')
scores$comb <- cover$SC

#add vectors
vec.df<-as.data.frame(scores(sev.fit, display="vectors"))
names <- c("Storrie severity (RdNBR)***", "Chips severity (RdNBR)**", "Combined severity", "Interaction**")
vec.df$names <- names
vec.df <-cbind(vec.df, names=rownames(vec.df))

ggplot()+
  geom_point(data=scores,aes(x=NMDS1,y=NMDS2, colour=scores$comb),size=4) + 
  scale_color_brewer(palette = "Set1", name="Severity combinations")+
  theme_classic()+
  theme (panel.border = element_rect(fill = NA))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title = element_text(size=18), axis.text = element_text(size=12, color="black"))+
  theme(legend.text = element_text(size=12), legend.title = element_text(size=16),
        legend.background = element_blank())+
  ggtitle("NMDS with burn serverity combination providing color coding")+
  geom_segment(data=vec.df,aes(x=0,xend=NMDS1,y=0,yend=NMDS2),
               arrow = arrow(length = unit(.5, "cm")), colour="blue", size=1.5) + 
  geom_text_repel(data=vec.df,aes(x=NMDS1,y=NMDS2,label=names), size=5, fontface="bold", point.padding = 1.6)+
  coord_fixed()+
  theme(axis.text = element_blank(), axis.ticks = element_blank())+
  annotate("text", x=-.48, y=.75, label="CEIN", size=5, fontface="bold", hjust=0)+
  annotate("text", x=.6, y=-.7, label="CECO", size=5, fontface="bold", hjust=0)+
  annotate("text", x=-.5, y=-.9, label="QUKE", size=5, fontface="bold", hjust=0)+
  annotate("text", x=-1, y=-.25, label="PSME", size=5, fontface="bold", hjust=0)+
  annotate("text", x=-1.3, y=-.9, label="ABCO", size=5, fontface="bold", hjust=0)
  
  
#old plot just for code  
#ggplot(midstory2, aes(chips_rdnbr))+
  geom_col(aes(y=PSME), fill="red", position=position_dodge(width=5), width = 15)+
  geom_col(aes(y=ABCO), position = "dodge", fill="grey50", color="black", width = 15)+
  scale_x_continuous(breaks=seq(-500,999,125))+
  coord_cartesian(xlim=c(-470, 900))+
  scale_y_continuous(limits = c(0,61), expand = c(0, 0))+
  theme_classic()+
  theme (panel.border = element_rect(fill = NA))+
  geom_vline(xintercept = 69)+
  geom_vline(xintercept = 315)+
  geom_vline(xintercept = 641)+
  geom_text(x=40, y=30, angle=90, label = "Unburned")+
  geom_text(x=290, y=30, angle=90, label = "Low") +
  geom_text(x=615, y=30, angle=90, label = "Moderate")+
  geom_text(x=930, y=30, angle=90, label = "High")+
  labs(y="Sapling density", x="Chips fire severity (RdNBR)")+
  theme(axis.title = element_text(size=20), axis.text = element_text(size=12, color="black"), axis.title.x = element_text(margin=margin(t=18)), axis.title.y = element_text(margin=margin(r=22)))+
  theme(legend.position = c(.09,.85), legend.title = element_blank(), legend.text = element_text(size=12), legend.background = element_blank())+
  annotate("text", x=875, y=55, label="b)", size=5)  
