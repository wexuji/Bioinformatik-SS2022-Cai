library(phangorn) 
library(ape)
fdir <- system.file("extdata/trees", package = "phangorn") 
primates <- read.phyDat(file.path(fdir, "primates.dna"),format = "interleaved")

#UPGMA Methode
#df_primates <- data.frame(primates)
dm <- dist.ml(primates) #Berechnen der Distanzmatrix
upgma_tree <- upgma(dm) #UPGMA Tree


#NJ-Methode
nj_tree <- NJ(dm)
unj_tree <- UNJ(dm)
plot(unj_tree, "unrooted")

