lmBoot <- function(inputData, nBoot){
  
  bootResults <- matrix(nrow = 2,ncol = 2* nBoot)
  
  for(i in 1:nBoot){
    
    # resample our data with replacement
    bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
    
    # fit the model under this alternative reality
    
    
    bootLM <- lm(inputData$Oxygen ~ inputData$Age, data = bootData)
  
    bootResults[i] <- coef(bootLM)
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
  bootResults <- matrix(nrow = nBoot, ncol = 2)
  for(i in 1:nBoot){
    bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
    bootLM <- lm(inputData$Oxygen ~ inputData$Age, data = bootData)
    bootResults[i] <- coef(bootLM)
    
  }
  bootResults
}
