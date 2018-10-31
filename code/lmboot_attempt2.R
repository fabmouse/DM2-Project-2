#Fitness data for testing
fitData <- read.csv("data/fitness.csv")

testData <- fitData[, c(3, 1, 2)] #Test data with response as the first variable and covariates of interest after
testData2 <- fitData %>%
                select(Oxygen, everything())

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

#GENERAL DOCUMENTATION:
#Purpose: Generate a large number of linear regression beta coefficients using
#         bootstrap methods.
#Inputs: inputData: a dataframe containing the response variable, which must be 
#        in the first column of the dataframe, and the covariates of interest
#        nBoot: the number of bootstrap samples to generate.
#Outputs: an array of nBoot beta coefficients for the intercept and covariates.

lmBoot_2 <- function(inputData, nBoot){
  #Changes: Calculates the Beta coefficients using matrix notaion rather than lm
  
  #Scale centers and/or sclaes the columns of a numeric matrix
  X <- cbind(1, scale(inputData[, -1], scale = F)) #Turns Covariates into a matrix and adding 1 for the intercept term
  Y <- scale(inputData[, 1], scale = F)            #Turns the response into a matrix
  scaleData <- as.matrix(cbind(Y, X))              #Final matrix with Response in first column and intercept and covariates in the rest
  
  bootResults <- array(dim = c(nBoot, ncol(X)))    #Build empty array to put results in
  for(i in 1:nBoot){
    # resample our data with replacement
    bootData <- scaleData[sample(1:nrow(inputData), nrow(inputData), replace = T),]  #Sample from 1:nrow of input data. Like random numbers to reassign
    Xmat <- bootData[, -1]                         #Separate into x and y matrices
    Ymat <- bootData[, 1]
    
    # fit the model under this alternative reality using matrix form rather than lm
    beta <- solve(t(Xmat)%*%Xmat)%*%t(Xmat)%*%Ymat 
    bootResults[i,] <- beta
  } # end of i loop
  colnames(bootResults) <- c("Intercept", names(inputData[-1]))
  bootResults
}

lmBoot_2(testData, 10)
set.seed(1234)
system.time(lmBoot_2(testData, 100000))    #user  system elapsed 
                                           #5.658   0.136   5.851

#Create a function for bootstrapping algorithm to use inside other functions
bootLM <- function(inputData, index){
  bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
  Xmat <- bootData[, -1]
  Ymat <- bootData[, 1]
  beta <- solve(t(Xmat)%*%Xmat)%*%t(Xmat)%*%Ymat
  return(t(beta))
}

lmBoot_3 <- function(inputData, nBoot){
  #Changes: 1. Uses a small function to carry out the bootstrap algorithm
  #         2. Uses sapply instead of a for loop
  
  #Scale centers and/or sclaes the columns of a numeric matrix
  X <- cbind(1, scale(inputData[, -1], scale = F)) 
  Y <- scale(inputData[, 1], scale = F)            
  scaleData <- as.matrix(cbind(Y, X))
  
  #Create an empty array to store results
  bootResults <- array(dim = c(nBoot, ncol(X))) 

  #Use apply to apply to bootResults matrix
  bootResults <- sapply(1:nBoot, bootLM, inputData = scaleData)
  
  #colnames(bootResults) <- c("Intercept", names(inputData[-1]))
  t(as.matrix(bootResults))
}

#set.seed(1234)
#lmBoot_2(testData, 5)
#set.seed(1234)
#lmBoot_3(testData, 5)

set.seed(1234)
system.time(lmBoot_3(testData, 100000))     #user  system elapsed 
                                            #5.346   0.138   5.510  

#Parallelisation: --------------------------------------------------------------
library(doParallel)

lmBoot_4 <- function(inputData, nBoot){

  
}


#RUI WORK: ---------------------------------------------------------------------
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




