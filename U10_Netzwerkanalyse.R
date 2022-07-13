#install.packages("igraph")
setwd("D:/University/FU_Berlin/Bachelor_Biochemie/SoSe2022/Bioinformatik/R/Übungsstunden")

library(igraph)
edges <- read.csv("U10_proteinInteractionNetworkHomoSapiensSmall.csv")
#Werte zusammenfassen, neue Variable 
#edgelist <- paste0(from","to)

x <- edges$FROM
y <- edges$TO

#datam <- as.matrix(edges)

edgelist <- paste0(x, "-", y)

help(csv_to_igraph)
help(graph)
