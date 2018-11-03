#WHAT TO DO:
# 1. Modify the the R bootstrapping code to be more efficient. 
#    It should also be parallelised at some level. 
#    You should profile both the original and final versions as well as determining the overall speed increase. 
#    Include the profile file in your repository.
#2. Your new bootstrap function in R should be altered to accept an arbitrary number of covariates.
#3. Micro-benchmark (package microbenchmark) your R bootstrap against bootstraps via the package boot.

# ORIGIONAL LMBOOT FUNCTION (CARL) ----------------------------------------
lmBoot <- function(inputData, nBoot){
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

# IMPROVEMENTS: ----------------------------------------------------------------
#Create a function for bootstrapping algorithm to use inside other functions
bootLM <- function(inputData, index){
  bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
  Xmat <- bootData[, -1]
  Ymat <- bootData[, 1]
  beta <- solve(t(Xmat)%*%Xmat)%*%t(Xmat)%*%Ymat
  return(t(beta))
}
lmBoot_3 <- function(inputData, nBoot){
  #Purpose: Generate a large number of linear regression beta coefficients using
  #         bootstrap methods.
  #Inputs: inputData: a dataframe containing the response variable, which must be 
  #        in the first column of the dataframe, and the covariates of interest
  #        nBoot: the number of bootstrap samples to generate.
  #Outputs: BootResults: An arraycontaing the parameter estimates of each 
  #         each bootstrap sample.
  #         ConfidenceIntervals: A matrix containing 95% confidence intervals 
  #         for each parameter.
  
  #Changes: 1. Allows for multiple covariates
  #         2. Calculates the Beta coefficients using matrix notaion rather than lm
  #         3. Uses a small function to carry out the bootstrap algorithm
  #         4. Uses sapply instead of a for loop
  
  #Create a sample dataset with a column of 1s for the intercept
  X <- cbind(1, inputData[, -1]) 
  sampleData <- as.matrix(cbind(inputData[, 1], X))
  
  #Create an empty array to store results
  bootResults <- array(dim = c(nBoot, ncol(X))) 

  #Use sapply to apply bootLM to bootResults matrix
  bootResults <- sapply(1:nBoot, bootLM, inputData = sampleData)
  
  #colnames(bootResults) <- c("Intercept", names(inputData[-1]))
  bootResults <- t(as.matrix(bootResults))
  
  #Calcluate confidence intervals
  ciMatrix <- matrix(NA, nrow = ncol(X), ncol = 2)
  for(i in 1:ncol(X)){
    ciMatrix[i, ] <- quantile(bootResults[,i], probs = c(0.025, 0.975))
  }
  colnames(ciMatrix) <- c("2.5%", "97.5%")
  rownames(ciMatrix) <- c("Intercept", names(testData[-1]))
  
  return(list(BootResults = bootResults,
              ConfidenceIntervals = ciMatrix))
}
                                            
# Parallelisation: --------------------------------------------------------------
#install.packages(doParallel)
library(doParallel)
lmBoot_4 <- function(inputData, nBoot){
  X <- cbind(1, inputData[, -1]) 
  sampleData <- as.matrix(cbind(inputData[, 1] , X))
  
  nCores <- detectCores()
  myClust <- makeCluster(nCores - 1, type = "PSOCK")
  registerDoParallel(myClust)
  
  bootResults <- array(dim = c(nBoot, ncol(X)))
  bootResults <- parSapply(myClust, 1:nBoot, bootLM, inputData = sampleData)
  
  #bootResults <- plyr::ldply(bootResults)
  stopCluster(myClust)
  bootResults <- t(bootResults)
  
  #Confidence intervals
  ciMatrix <- matrix(NA, nrow = ncol(X), ncol = 2)
  for(i in 1:ncol(X)){
    ciMatrix[i, ] <- quantile(bootResults[,i], probs = c(0.025, 0.975))
  }
  colnames(ciMatrix) <- c("2.5%", "97.5%")
  rownames(ciMatrix) <- c("Intercept", names(inputData[-1]))
  
  return(list(BootResults = bootResults,
              ConfidenceIntervals = ciMatrix))
  
}

# Testing Output and Timing -----------------------------------------------
fitData <- read.csv("data/fitness.csv")

#Testing datasets with response as the first variable and covariates of interest after
library(dplyr)
testData <- fitData[, c(3, 1, 2)]
testData2 <- fitData %>%
  select(Oxygen, everything())

#Carls original function
x <- fitData$Oxygen
y <- fitData$Age
system.time(lmBoot(data.frame(x, y), 10000))

#Imporved function
set.seed(1234)
lmBoot_3(testData, 5)
system.time(lmBoot_3(testData, 100000)) 

#Parallised function
set.seed(1234, "L'Ecuyer") #Add "L'Ecuyer" to make it reproduceable
lmBoot_4(testData, 5)
system.time(lmBoot_4(testData, 100000)) 

# System.Time Results -----------------------------------------------------
#THIS DOESN't WORK
# timingMatrix <- matrix(NA, nrow = 5, ncol = 3)
# for(i in 1:5){
#   timingMatrix[i, ] <- c(system.time(lmBoot(data.frame(x, y), 10)),
#                           system.time(lmBoot_3(testData, 10)), 
#                           system.time(lmBoot_3(testData, 10)))  
# }

# Profiling ---------------------------------------------------------------
install.packages("profvis")
library(profvis)

profvis({lmBoot_2(testData, 1000)})
profvis({lmBoot_3(testData, 1000)})
profvis({lmBoot_4(testData, 1000)})

# RUI WORK: ---------------------------------------------------------------------
# Add multiple covariates
# Do some more optimisations (do the resampling in parallel or only once)
# You can still do sime changes to the bootLM output so that you can get rid of the transpose and plyr::ldply(bootResults)

bootLM <- function(inputData, index){
  bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
  Xmat <- bootData[,1:2]
  Ymat <- bootData[,3]
  
  beta <- solve(t(Xmat)%*%Xmat)%*%t(Xmat)%*%Ymat
  return(t(beta))
}

bootLM(fitData, 10)

badBootBrother2 <- function(inputData, nBoot){
  X <- cbind(1, inputData$x)
  dataSet <- as.matrix(cbind(X, inputData$y))
  
  nCores <- detectCores()
  myClust <- makeCluster(nCores - 1, type = "PSOCK")
  registerDoParallel(myClust)
  
  bootResults <- array(dim=c(nBoot, 2))
  bootResults <- parLapply(myClust, 1:nBoot, bootLM, inputData = dataSet)
  
  bootResults <- plyr::ldply(bootResults)
  stopCluster(myClust)
  return(bootResults)
  
}

# Testing
x <- fitness$Weight
y <- fitness$Age
test.data <- data.frame(x, y)
badBootBrother2(test.data, 10)




