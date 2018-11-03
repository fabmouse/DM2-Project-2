library(microbenchmark)
library(boot)
#source("lmboot_attempt2.R")

microbenchmark(lmBoot_4(fitData,5),times = 10L)

bst <- function(formula, data, indices){
  d <- data[indices, ]
  fit <- lm(formula, data=d)
  return(coef(fit)) 
}



microbenchmark(boot(data = fitData, statistic = bst, R =5, formula = Oxygen~.),times = 10L)
