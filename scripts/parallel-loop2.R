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
    #Using foreach to run the simTweedieTest M times in parallel
  pvalues <- foreach(m = 1:M, .combine = c, 
                     .packages = "tweedie",
                     .export = "simTweedieTest") %dopar% {
                       simTweedieTest(N)
                     }
  sum(pvalues < sig) / M
  }

df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 


for(i in 1:nrow(df)){ 
  df$share_reject[i] <-  
    MTweedieTests( 
      N=df$N[i], 
      M=df$M[i], 
      sig=.05) 
} 

#Stopping the cluster
stopCluster(cl)