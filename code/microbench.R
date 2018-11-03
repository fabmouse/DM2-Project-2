library(microbenchmark)
library(boot)
#source("lmboot_attempt2.R")
set.seed(5763)

<<<<<<< HEAD
microbenchmark(lmBoot_4(fitData,10000),times = 10L)
=======
microbenchmark(lmBoot_4(fitData,5),times = 10L)
>>>>>>> 8d367b8263d79e6deb3fcbc430b200b50600e1f0

bst <- function(formula, data, indices){
  d <- data[indices, ]
  fit <- lm(formula, data=d)
  return(coef(fit)) 
}



<<<<<<< HEAD
microbenchmark(boot(data = fitData, statistic = bst, R =10000, formula = Oxygen~.),times = 10L)
=======
microbenchmark(boot(data = fitData, statistic = bst, R =5, formula = Oxygen~.),times = 10L)
>>>>>>> 8d367b8263d79e6deb3fcbc430b200b50600e1f0
