p_laplace <- function(n, p){
  set.seed(1)                                 #for replicatable results
  trial <- sample(p, n, rep = T)              #generates random trial with n numbers
  heads <- sum(trial == 1)              
  success <- heads/n                          #calculates propability for success from sample
  
  laplace <- (heads + 1)/(n + 2)              #laplace propability for n+1 from results
  
  #Calculation of p(n+1) witout laplace from sample data
  n_plus1 <- sample(p, 1, rep = T)        
  n_head <- sum(n_plus1 == 1)
  n_success<- (heads+n_head)/(n+1)
  
  values <- c(laplace, n_success)
  print(paste0("laplace: " , laplace))
  print(paste0("propability: " , n_success))
  return(laplace)
}

p_laplace(10, 0:1)
p_laplace(100, 0:1)
p_laplace(1000, 0:1)
p_laplace(10000, 0:1)
