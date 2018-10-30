library(foreach)
library(parallel)
library(doParallel)
registerDoParallel(cores=detectCores(all.tests=TRUE))
# define helper functions

lmBoot <- function(inputData, nBoot){
  
  # bootResults <- array(dim=c(nBoot, 2))#
  
  for(i in 1:nBoot){
    
    # resample our data with replacement
    bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
    
    # fit the model under this alternative reality
    bootLM <- lm(y ~ x, data = bootData)
    
    # store the coefs
    if(i == 1){
      
      bootResults <- matrix(coef(bootLM), ncol = 2)
      
    } else {
      
      bootResults<- rbind(bootResults, matrix(coef(bootLM), ncol = 2))
      
    }
    
    
  } # end of i loop
  
  bootResults
  
}

boot <- function(inputData){
  boot <- coef(lm(y ~ x, data = inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]))
}


fitness <- read.csv("data/fitness.csv")
df <- data.frame(y = fitness$RunTime,x = fitness$Weight)
start1 <- Sys.time()

lmBootSu <- function(inputData, nBoot){
  #bootResults <- matrix(NA,nrow = nBoot, ncol = 2)
  
  for(i in 1:nBoot){
    #bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
    #print(bootData)
    #bootLM <- lm(y ~ x, data = inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),])
    #bootResults[i,] <- coef(bootLM)
    bootResults[i,] <- coef(lm(y ~ x, data = inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]))
    
  }
  
  #boot[i,] <- coef(lm(y ~ x, data = inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]))
  #bootData <- apply
  
  #bootResults <- apply(inputData,bootResults,boot)
  #bootResult <- apply(1:nBoot, function(x){ coef(lm(y ~ z, data = x[sample(1:nrow(x), nrow(x), replace = T),])) })
  bootResults
}

