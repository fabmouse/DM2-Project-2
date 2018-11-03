library(microbenchmark)
library(boot)

microbenchmark(
  lmBoot_4(fitData,1000)
)

microbenchmark(
  boot(fitData,)
)

lmBoot_4(fitness,1000)
bst <- function(formula, data, indices){
  d <- data[indices, ]
  fit <- lm(formula, data=d)
  return(coef(fit)) 
}
bootmodel = lm(Oxygen ~.,data = fitData)
boot(data = fitData,statistic = bst. R = 1000,formula = bootmodel)
