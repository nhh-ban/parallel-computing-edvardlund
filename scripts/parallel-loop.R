#oading packages 
library(tweedie) 
library(tidyverse)
library(doParallel)

#Determining the number of coress to use 
maxcores <- 8
Cores <- min(parallel::detectCores(), maxcores) 

#Instatiating the cores
cl <- makeCluster(Cores)
registerDoParallel(cl)


simTweedieTest <-  
  function(N){ 
    t.test( 
      rtweedie(N, mu=10000, phi=100, power=1.9), 
      mu=10000 
    )$p.value 
  } 


MTweedieTests <-  
  function(N,M,sig){ 
    sum(replicate(M,simTweedieTest(N)) < sig)/M 
  } 


df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 

# Parallel loop using foreach, rewriting original for loop
results <- foreach(i = 1:nrow(df), .combine = 'rbind', 
                   .packages = c('tweedie','tibble')) %dopar% {
  tibble(
    N = df$N[i],
    M = df$M[i],
    share_reject = MTweedieTests(N=df$N[i], M=df$M[i], sig=.05)
  )
}

# Assign results to df
df$share_reject <- results$share_reject

#Stopping the cluster
stopCluster(cl)

