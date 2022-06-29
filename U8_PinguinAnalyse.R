#install.packages("palmerpenguins")
#install.packages("factoextra")
install.packages("cluster")

#loading the penguins
library(palmerpenguins)

head(penguins)
head(penguins_raw)

library(tidyverse)

#showing structure of data frames
str(penguins)
str(penguins_raw)

penguins_numbers <- penguins[ , c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")]
penguins_numbers <- na.omit(penguins_numbers)

#PCA Analyse----
#calculate principal components, scale = TRUE -> sets everything to sd = 1 and mean = 0
peng_pr <- prcomp(penguins_numbers, center = TRUE, scale= TRUE)
summary(peng_pr)

#cumulative properties from summary: 
#for PC2 = 0.88 -> using 2 components 88% of variance can be shown

#plot for showing variance with Eigenvalue > 1
screeplot(peng_pr, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5) #Line at height = 1
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6) #legend plcement

#explains amount of explained variance
cumpro <- cumsum(peng_pr$sdev^2 / sum(peng_pr$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)

#plots show that first 2 dimensions retain ca. 90% of variance
#--> reduced dimensionality from 4 to 2 

plot(peng_pr$x[,1],peng_pr$x[,2], xlab="PC1 (68.8%)", ylab = "PC2 (19.3%)", main = "PC1 / PC2 - plot")

#k-means clustering-----
library(factoextra, cluster)

#scale penguin variables for mean = 0 and sd = 1
peng_k <- scale(penguins_numbers)

head(peng_k)

#finding out the optimal k 
fviz_nbclust(peng_k, kmeans, method = "wss")

#set seed for reproducability 
set.seed(1)

#set k means analysis with 2 centers and 25 random starting positions
km <- kmeans(peng_k, centers = 2, n = 25)
km
#plotting the clusters
fviz_cluster(km, data = peng_k)
