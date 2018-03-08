# Determine the best number of groups by assessing the mean indicator values for all species
require(indicspecies)

#data frame of response variables (ALL common species importance values)
dat2 <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/importance.csv")
# dataframe of predictor variables (rdnbr, plot names & categories)
cover <- read.csv(file="C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/ccover1.imp.csv")

#These are where the results will be stored
mean_indcls <- numeric(20) #mean indicator species stat
num_spp <- numeric(20) #number of significant indicator species
avg_pval <- numeric(20) #average pvalue

require(vegan)
cover2.dist <- vegdist(cover2, method = "horn")
#clusters
x <- hclust(d=cover2.dist, method = "ward.D2")

for (k_cuts in 2:20)
{
  cut_tree<-cutree(x,k=k_cuts)
  ind_species<-multipatt(dat2,cut_tree,duleg=TRUE)
  x<-ind_species
  sel <- !is.na(x$sign$p.value) & x$sign$p.value <= .05 #check which species are significant
  a<-x$sign[sel, ] 
  ncolsign = ncol(x$sign)
  num_spp[k_cuts]<-sum(rowSums(a[, 1:(ncolsign - 3)]) == 1)
  mean_indcls[k_cuts]<-mean(ind_species$sign$stat)
  avg_pval[k_cuts]<-mean(ind_species$sign$p.value)
 
}

#plot the results
plot(num_spp)
abline(v=match(max(num_spp),num_spp), lty=3, lwd=2, col="Blue")
plot(mean_indcls)
abline(v=match(max(mean_indcls),mean_indcls), lty=3, lwd=2, col="Blue")
plot(avg_pval)
abline(v=match(min(avg_pval[-1]),avg_pval), lty=3, lwd=2, col="blue")
#hopefully some of these will match up. Usually highest num_spp and lowest avg_pval are around the same number of cuts 