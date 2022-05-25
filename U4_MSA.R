# Ich habe versucht, alle vier Schritte des Multiple Sequence Alignments selbst
# zu programmieren, also:
#   1. den Needleman-Wunsch-Algorithmus, wobei ich dafür ein Package genutzt habe
#   2. die Berechnung der p-Distanz
#   3. Die Erstellung des Baums und 
#   4. Das Sequence-Alignment und das Erstellen der Konsenssequenz. 
#   
# Dabei habe ich es bisher nur geschafft, den Needleman-Wunsch Algorithmus für alle
# Pairwise-Alignments anzuwenden und alle p-Distanzen zu berechnen. Die Erstellung des 
# Baums scheiterte daran, die Distanzmarix aus den p-Distanzen zu erstellen (ich habe 
# es nicht geschafft, die Distanzen in einen Datensatz einzubringen bzw war mir nicht sicher,
# ob ich es als Dataframe oder long list machen sollte, da NJ und UPGMA mit einer
# long list als dm arbeiten, wobei ich auch nicht finden konnte, wie eine long list erstellt wird). 
# 
# Das Erstellen der Konsenssequenz ist in U4_MSA_neu zu sehen. 
# 
# Das, was ich bisher geschafft habe, hat mich 6 Stunden gekostet, daher glaube ich nicht, 
# dass ich diese Datei zuende schreiben werde, vor allem, weil ich mir auch 
# Gedanken darüber gemacht habe, wie ich alles in eine Funktion verpacke und das unmöglich erscheint. 
# Nichtsdestotrotz hätte ich gerne eine Bestätigung darüber, ob zumindest meine Ansätze richtig sind. 





library(stringr)
library(NameNeedle)
setwd("D:/University/FU_Berlin/Bachelor_Biochemie/SoSe2022/Bioinformatik/R/Übungsstunden")

#Einlesen der Daten als Datensatz --------------
seq <- read.table("human_ACTG_Isoforms.txt") 
seq <- t(seq)
seq <- as.data.frame(seq) #Umwandeln in Data Frame


#Umwandlen der Daten als Schleife -------------------

#Hierbei hatte ich ja die Frage darüber, wie ich eine Variable loopen kann, sodass alles 
#in seq1 - seq9 gespeichert wird bzw wie das verallgemeinert werden kann (z.B. bei 100 Variablen). 
#Die Lösung mit der Liste in der Liste weiß ich jetzt ja auch, aber falls es trotzdem geht, 
#würde ich mich über eine Lösung freuen :)

lista <- c()
seq_list <- list(seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9) 
for(a in 1:ncol(seq)){
  #temp <- strsplit(seq[[a]], "")
  assign(paste0("seq",a), temp)
  lista<- c(lista, temp)
  #rm(temp)
}


#hier hatte ich ein ähnliches Problem, dass ich die Variablen automatisieren wollte. 
#Ich wollte die Columns im DF neubenennen, hatte ursprünglich die Idee, dass ich 
#auch durch eine for Schleife die Namen seq1 - seq9 erstelle, als ich die 
#Liste aber als Namen verwenden wollte, wurden allen Spalten nur die Werte seq1-seq9 
#zugewiesen. 

#Den Versuch hab ich rausgelöscht, aber wenn es eine bessere Möglichkeit gibt, freu ich mich
#natürlich über Anstöße.

seq_list <- list(seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9) 
seq_df <- data.frame(seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9)
names(seq_df) <- list("seq1", "seq2", "seq3", "seq4", "seq5", "seq6", "seq7", "seq8", "seq9")


rm(seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9)


#Einlesen der Daten als Liste ----------
seq_nw <- read.table("human_ACTG_Isoforms.txt") #sequence unsplit
seq_nw <- t(seq_nw)
seq_nw<- as.data.frame(seq_nw)

rm(seq_ns, seq_list)
#Needleman-Wunsch---------------

#Hier sind das echt unübersichtlich viele Variablen
#assigne ich aber nicht l = ncol(seq_nw)-1, so bekomme ich einen out of bounds Fehler. 

l <- ncol(seq_nw)-1
for(i in (1:l)){
  k <- i+1
  #assign(paste0("S", i), list())
  for (j in k:ncol(seq_nw)){
    score <- needles(seq_nw[[i]], seq_nw[[j]])$score
    print(score)
    #assign(paste0("S", i)[j], score)
    assign(paste0("score_",i, j), score)
  }
}
rm(i, j, k, l)


#Needleman-Wunsch als Funktion------
#das war der Versuch, meine Schleife zu verallgemeinern für einen gesamten Datensatz
#sodass nicht auf jedes Pairwise alignment Needleman-Wunsch angewandt werden musste 

needle_func <- function(df, i){
  nlist <- list(0)
  for (j in (i+1:ncol(df))) {
    score <- needles(df[[i]], df[[j]])$score
    print(score)
    assign(paste0("score", j), score)
  }
  return(list(score1, score2, score3))
}


#Die Scores haben in der Environment unübersichtlich gewirkt, deswegen weg damit
rm(score_12, score_13, score_14, score_15, score_16, score_17, score_18, score_19)
rm(score_23, score_24, score_25, score_26, score_27, score_28, score_29)
rm(score_34, score_35, score_36, score_37, score_38, score_39)
rm(score_45, score_46, score_47, score_48, score_49)
rm(score_56, score_57, score_58, score_59)
rm(score_67, score_68, score_69)
rm(score_78, score_79)
rm(score_89)

#ich glaube das war die Vorbereitung für das Erstellen der Matrix, aber mir ist mittendrin aufgefallen
#dass ich das ganze nicht brauche. 
#Es existiert weiterhin für eventuelle weiterverwendungsmöglichkeiten. 
S1 <- list(score_12, score_13, score_14, score_15, score_16, score_17, score_18, score_19)
S2 <- list(score_23, score_24, score_25, score_26, score_27, score_28, score_29)
S3 <- list(score_34, score_35, score_36, score_37, score_38, score_39)
S4 <- list(score_45, score_46, score_47, score_48, score_49)
S5 <- list(score_56, score_57, score_58, score_59)

rm(S1, S2, S3, S4, S5, S6, S7, S8)
rm(score)

list1 <- needle_func(seq_ns, 3)
list2 <- needle_func(seq_ns, 2)


#Berechnung der p-Distanz ----------------------

#p-Distanz als Funktion----
p_distance <- function(df, a, b){
  counter <- 0
  
  df$equal <- df[[a]] == df[[b]]  #Vektor erstellen, der zählt, welche enträge gleich sind
  counter <- subset(df$equal, df$equal == TRUE) #Zählt die Anzahl der gleichen Einträge
  pd <- length(counter)/length(df[[1]]) 
  pd <- round(pd, 3)
}

#p-Distance loop test ----
h <- ncol(seq_df)-2
i <- ncol(seq_df)-1
for(j in 1:h){
  k <- j+1
  for(l in k:i){
    assign(paste0("p", j, l), p_distance(seq_df, j, l))
    print(j)
    print(l)
    print("hallo")
  }
}

rm(p110, p210, p310, p410, p510, p610, p710, p810, p910)
rm(pd)

#Distanzmatrix----
#Das hat gar nicht funktioniert
#Ich wollte das zuerst als long list machen (Dateiformat der Distanzmatrix des Package
#für die Erstellung der phylogenetischen Bäume), aber ich konnte nicht finden, wie das funktioniert
#bei einem Dataframe wüsste ich auch nicht, ob das package "phangorn" sie als distanzmatrix akzeptiert

dm <- data.frame(
  d1 = c(0, p_12, p_13, p_14)
)

#Theoretisch kämen noch Baum, MSA, Lücken und Konsenssequenz, aber bis dahin bin ich nicht gekommen
#wahrscheinlich hätte ich auch hier mit weiteren packages wie Phangorn gearbeitet 
#für das richtige Alignment (mit einfügen von Lücken und so) hätte ich auch nach einem Package gesucht
