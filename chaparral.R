#######################################
#This script looks at montane chaparral dynamics in an area of the Lassen National Forest, CA that has experienced two subsequent wildfires (Storrie Fire in 2000 and Chips Fire in 2012)
# It compares the cover of shrub species that typically sprout after fire to those that rely on fire stimulated germination for post-fire recovery

#sets working directory
setwd("~/Grad School/LASSEN PROJECT/for R")

#imports the dataset of shrub cover 
shrub.cover.center <- read.csv("~/Grad School/LASSEN PROJECT/for R/shrub cover center.csv")

#loads packages and opens libraries necessary to run this script
install.packages("agricolae")
library(agricolae)
install.packages ("plyr")
library(plyr)

#import variables
#imports fire severities of each fire as character vectors
storsev <- (as.character(shrub.cover.center$storsev))
chipsev <- (as.character(shrub.cover.center$chipsev))

#combines percent cover of sprouter spp ARPA, CEVE and SYMO into a single variable and imports that as a vector, and turns proportions into percents
sprouters <- (rowSums(shrub.cover.center[,c(10,15,28)], na.rm=TRUE))*100
#combines percent cover of seeder spp CECO and CEIN into a single variable and imports that as a vector, and turns proportions into percents
seeders <- (rowSums(shrub.cover.center[,c(12, 13)], na.rm=TRUE))*100

#conducts transformations of the response variables
tsprout <- (sqrt(sprouters))
tseed <- (sqrt(seeders))

#groups severities for both fires into two classes, severities 1&2="low" and severities 3&4="high"
sevs <- revalue(storsev, c("1"="low", "2"="low", "3"="high", "4"="high"))
sevc <- revalue(chipsev, c("1"="low", "2"="low", "3"="high", "4"="high"))

#combines severity classes from each fire (e.g., low in Storrie, low in Chips = "low/low") to create fire severity combinations for analysis
stochip <- paste (sevs, sevc, sep="/") 

### forces r to order the variables in the following manner ("low/low", "low/high", "high/low", "high/high")
test <- factor (stochip, ordered = TRUE, levels=c("low/low", "low/high", "high/low", "high/high"))

#######################################################
#Graphs 

#sets R to create a seperate graphing window, and to put both plots in the same window
X11(width=10,height=6)
par(mfrow=c(1,2))

#creates a boxplot of sprouter percent cover by severity combination using untransformed data for visual clarity, pvalues are for transformed data
boxplot (sprouters~test, ylab="% Cover", main="Sprouting species", xlab="Combined fire severity", cex.lab=1.3, cex.main=1.5,  ylim=c(0, 105), col=c( "green4", "gold1", "orangered","red4"), ces.lab=1.1)
text(2, 100, "A")
text(1, 100, "A")
text(3, 100, "B",font=2)
text(4, 100, "A")
text(4.3, 104.8, "p<.0001", cex=.8, font=2)

#creates a boxplot of seeder percent cover by severity combination using untransformed data for visual clarity, pvalues are for transformed data
boxplot (seeders~test, ylab="% Cover", main="Seeding species", xlab="Combined fire severity", cex.lab=1.3, cex.main=1.5, col=c("green4", "gold1", "orangered","red4"), ylim=c(0,105))
text(1, 102, "A", font=2)
text(2, 102, "B")
text(3, 102, "B")
text(4, 102, "B")
text(4.3, 104.9, "p<.0001", cex=.8, font=2)

#statistical analysis (ANOVA) of relationship of sprouter spp % cover to burn combination
sprouts <- lm(tsprout~stochip-1)
anova (sprouts)
summary (sprouts)

#conducts a Tukey's multiple comparison test to compare combinations
HSD.test(sprouts, 'stochip')
print (HSD.test(sprouts, 'stochip'))

#statistical analysis (ANOVA) of relationship of seeder spp % cover to burn combination
seeds <- lm(tseed~stochip-1)
anova (seeds)
summary (seeds)

#conducts a Tukey's multiple comparison test to compare combinations
HSD.test(seeds, 'stochip')
print (HSD.test(seeds, 'stochip'))





