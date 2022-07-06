#install.packages("class")
#install.packages("ggplot2")
setwd("D:/University/FU_Berlin/Bachelor_Biochemie/SoSe2022/Bioinformatik/R/Übungsstunden")

library(ggplot2)
library(class)

#loading in patient data set
patient <- read.csv("genedata-for-knn.csv")
str(patient)

#plot----
ggplot(patient, aes(X1, X2, color = Sick)) + geom_point(size = 1) + ggtitle("Patientendaten Krebstumor")

#append new data ----
#new_row <- c(0, 0, NA)
#new_row <- c(0, 5, NA)
#new_row <- c(0, -8, NA)
new_row <- c(-5, 10, NA)

patient <- rbind(patient,new_row)
ggplot(patient, aes(X1, X2, color = Sick)) + geom_point(size = 2) + ggtitle("Patientendaten Krebstumor")

#remove freshly added rows
patient <- na.omit(patient)

#knn.cv -------
train <- subset(patient, select = -c(Sick)) #training data without nonßnumerical factors
cl <- patient$Sick #classification data

knn.cv(train, cl, k = 3, prob = TRUE)
attributes(.Last.value)

