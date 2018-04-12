### extra code

#from nmds and cluster analysis
#combine indicator species analysis with k means clustering####

#choose # of groups
ck <- cascadeKM(cover2, inf.gr = 2, sup.gr = 10) #evaluates # of groups 
plot(ck) #indicates that 4 groups are best?

#conduct a kmeans cluster analysis
k <- kmeans(cover2, centers = 4, nstart = 100)

#runs combined analysis 
#indicator species analysis with kmeans cluster determining groups
kind <- multipatt(cover2, k$cluster, control = how(nperm = 999))
summary(kind, indvalcomp = TRUE)
kind.val <- indval(cover2, k$cluster)
summary(kind.val)

#should I use the pam function, since the distance matrix above may not be appropriate?  
library(cluster)
#create a BC distance matrix for analysis
cov.dist <- vegdist(cover2, method = "bray")
asw <- numeric(10)
for (k in 2:10) {
  asw[k] <- pam(cov.dist, k = k) $ silinfo $ avg.width
}
cat("silhouette-optimal # of clusters:", which.max(asw), "\n")
k6.pam <- pam(x=cov.dist, k=6)
summary(k6.pam)
k6.pam$clustering
#######################################################
#ordination using cluster analysis to determine groups (not severity)####
z <- metaMDS(comm = cover2, k=3, distance = "horn", weakties=T, trymax = 100, autotransform = FALSE)
z.cluster <- MDSrotate(z, cover$storrie_rdnbr)
plot(z.cluster, display = "sites")
ordihull(z.cluster, groups = k$cluster, col = c("red", "green", "blue", "orange"))
text(z.cluster, labels = kind$cluster)
cluster.fit <- envfit(z.cluster ~ storrie_rdnbr + chips_rdnbr, data=cover, na.rm=T)
plot(cluster.fit)
clus <- as.factor(kind$cluster)


#ordicluster(ord=sev.fit, display = "sites", cluster=k, prune=3)

####mrpp
ex1 <- mrpp(cover2, grouping = k$cluster, distance = "bray") #A: 0.5729 
####################################################
#from indicator species

#check correlation usings Pearson's Phi
#convert cover data to a presence/absence matrix
#cover2pa <- as.data.frame(ifelse(cover2>0, 1, 0))
#phi <- multipatt(cover2pa, cat, func = "r.g", control=how(nperm=999), duleg = T)
#summary(phi)

########################################
#from permanova
#B-C distance matrix
cov.dist <- vegdist(cover2, method = "bray")

#conduct a kmeans cluster analysis
k <- kmeans(cover2, centers = 4, nstart = 100)

#PERMANOVA
#simple test for overall effect of cluster
try1 <- adonis(cover2~k$cluster, data = cover2)
try1


