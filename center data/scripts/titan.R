library(labdsv)
library(vegan)
library(TITAN2)

#data frame of response variables (all common species Relative cover values)
cover2 <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/coverRel.csv")

# dataframe of predictor variables (rdnbr, plot names & categories)
cover <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/cover1.csv")
#add dominant species for each plot

sevcats <- factor(cover$SC, ordered = T, levels=c("un/un", "un/low", "low/un", "low/low", "un/high", "low/high", "high/un", "high/low", "high/high"))
################################################
#PCA of Storrie & Chips severity
df <- data.frame(cover$storrie_rdnbr, cover$chips_rdnbr)
pc.rdnbr <- pca(df)
pc1 <- pc.rdnbr$scores[,1]
pc2 <- pc.rdnbr$scores[,2]

#creates NMDS ordination####
z <- metaMDS(comm = cover3, k=3, distance = "bray", weakties=T, trymax = 100, autotransform = FALSE)
z$stress

#PERMANOVA of abundance vs. overall severity
per1 <- adonis(cover2~pc.rdnbr$scores[,1])

per2 <- adonis(cover2~pc.rdnbr$scores[,2])
per2

plot(cover2$CECO~pc.rdnbr$scores[,2])

lm1 <- lm(cover2$CECO~pc.rdnbr$scores[,1])
summary(lm1)

per4 <- adonis(cover2~cover$SC)
per4

pairwiseAdonis::pairwise.adonis(cover2, cover$SC)
#################################################
#Titan??
tit <- titan(cover$storrie_rdnbr, cover2, nBoot = 50)

################################################
#make comparison boxplots for each group
seed <- (cover2$CEIN)
ceco <- cover2$CECO
fsprout <- cover2$CECO+cover2$ARPA+cover2$RIRO+cover2$CEVE+cover2$QUKE+cover2$SYMO
trees <- cover2$ABCO+cover2$PIPO+cover2$PILA+cover2$PSME
cols <- rainbow(9)

par(mfrow=c(1,2))
boxplot(seed~sevcats, ylab="Relative cover", cex=3, col=rev(cols))
points(seed~sevcats, pch=19)
title(main="Deerbrush")

boxplot(ceco~sevcats, ylab="Relative cover", col=rev(cols))
points(ceco~sevcats, pch=19)
title(main="Whitethorn")

boxplot(fsprout~cover$SC)
points(fsprout~cover$SC, pch=19)
title(main="Sprouters")

boxplot(trees~sevcats, col=rev(cols))
points(trees~sevcats, pch=19)
title(main="Conifers")

boxplot(cover2$QUKE~sevcats, col=rev(cols))
points(trees~sevcats, pch=19)
title(main="Black oak")