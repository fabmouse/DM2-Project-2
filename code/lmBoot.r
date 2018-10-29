# define helper functions
fitness <- read.csv("data/fitness.csv")
df <- data.frame(y = fitness$RunTime,x = fitness$Weight)

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


lmBootSu <- function(inputData, nBoot){
  bootResults <- matrix(NA,nrow = nBoot, ncol = 2)
  for(i in 1:nBoot){
    bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
    bootLM <- lm(y ~ x, data = bootData)
    bootResults[i,] <- coef(bootLM)
    
  }
  bootResults
}
