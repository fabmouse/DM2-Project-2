library(doParallel)

# Add multiple covariates
# Do some more optimisations (do the resampling in parallel or only once)
# You can still do sime changes to the bootLM output so that you can get rid of the transpose and plyr::ldply(bootResults)

bootLM <- function(index, inputData){
  bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
  Xmat <- bootData[,1:2]
  Ymat <- bootData[,3]
  
  beta <- solve(t(Xmat)%*%Xmat)%*%t(Xmat)%*%Ymat
  return(t(beta))
}

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
