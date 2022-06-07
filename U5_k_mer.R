
#Einlesen der Sequenz und Aufspalten in Liste
seq <- "ACGTTGCATGTCGCATGATGCATGAGAGCT"

#create Vector from strsplit
seq = unlist(strsplit(seq, ""))

#PatternCount function------

PatternCount <- function(text, pattern){
  counter <<- 0
  
  ntext <<- length(text) #text has to be a vector
  npattern <<- nchar(pattern)
  diff <- ntext - npattern
  npattern1 <- npattern -1 #for inner loop
  
  temp_pattern <- c()
  for(i in 1:diff){
    
    temp_pattern <- append(temp_pattern, text[i]) #ensures that first pattern is included
    
    for(l in 1:npattern1){ #loop for saving the right amount of letters in the patern
      temp_pattern <- append(temp_pattern, text[i+l])
    }
    
    temp_pattern <- paste(temp_pattern, collapse = "") #concats pattern into one single string
    #print(temp_pattern)
   # Sys.sleep(1)
    
    if (temp_pattern == pattern){
      counter <<- counter + 1
    }
    
    temp_pattern <- c()
  }
  return(counter)
}

FrequentPatterns <- function(seq, k){
  pattern <- c() #empty pattern
  patternList <- c() #empty list to save all patterns
  
  fPatterns <- c() #frequent Patterns
  nseq <- length(seq)
  temp <- nseq - k
  count <- c() #counter for number of times where patterns appear
  
  #loop 1----
  for(i in 1:temp){
    for (j in 1:k){
      pattern <- append(pattern, seq[i+j]) #temporary Pattern that is currently being observed
      #print(pattern)
      #Sys.sleep(2)
    }
    rm(j)
    patternList[i] <- paste(pattern, collapse = "") #concat all letters into a sequence
    
    counted <- PatternCount(seq, patternList[i]) #number of times a seq appears
    count <- append(count, counted)
    pattern <- c() #pattern set back to null
  }
  
  #print(patternList)
  #patternList <- c()
  
  #print(count)
  
  maxCount <- max(count)
  
  l <- 1
  for(l in 1:temp){
    if(count[l] == maxCount){
      fPatterns <- append(fPatterns, patternList[l]) 
    }
  }
  
  
  fPatterns <- fPatterns[!duplicated(fPatterns)]
  print(fPatterns)
}

#testing results
results <- c()
for(i in 1:5){
  loop <- FrequentPatterns(seq, i)
  results <- append(results, loop)
}