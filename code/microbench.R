library(microbenchmark)
library(boot)

microbenchmark(lmBoot_4(fitData,10))

bst <- function(formula, data, indices){
  d <- data[indices, ]
  fit <- lm(formula, data=d)
  return(coef(fit)) 
}



microbenchmark(boot(data = fitData, statistic = bst, R =10, formula = Oxygen~.))
