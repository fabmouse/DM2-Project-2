library(boot)
library(microbenchmark)
set.seed(5763)

bst <- function(formula, data, indices){
  d <- data[indices, ]
  fit <- lm(formula, data=d)
  return(coef(fit)) 
}

boot_microbenchmark <- microbenchmark(boot(data = fitData, statistic = bst, R = 10, formula = Oxygen~.))
lmboot_4_microbenchmark <- microbenchmark(lmBoot_4(fitData,10))

print(boot_microbenchmark)
print(lmboot_4_microbenchmark)
