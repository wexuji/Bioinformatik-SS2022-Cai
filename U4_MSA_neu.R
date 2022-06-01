library(stringr)
setwd("D:/University/FU_Berlin/Bachelor_Biochemie/SoSe2022/Bioinformatik/R/Übungsstunden")

#Einlesen der Daten als Datensatz --------------
seq <- read.table("human_ACTG_Isoforms.txt") 
seq <- t(seq)
seq <- as.data.frame(seq) #Umwandeln in Data Frame


#Umwandlen der Daten als Schleife -------------------

for(i in 1:ncol(seq)){
  temp <- strsplit(seq[[i]], "")
  assign(paste0("seq",i), temp)
  rm(temp)
}
rm(i)

seq_list <- list(seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9) 
seq_df <- data.frame(seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9)
names(seq_df) <- list("seq1", "seq2", "seq3", "seq4", "seq5", "seq6", "seq7", "seq8", "seq9")


rm(seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9)

seq_df <- t(seq_df) #damit alle Positionen aligned sind
seq_df <- as.data.frame(seq_df)

#Konsensussequenz-----

consens <- function(seq_df){
  consensus <- c()
  for (i in 1:ncol(seq_df)){
    sorted <- sort(seq_df[[i]], decreasing = TRUE)
    consensus <- c(consensus, sorted[1])
  }

  consens_seq <- paste(consensus, collapse = "")
  paste0("Die Konsenssequenz ist ", consens_seq)
}

consens(seq_df)
