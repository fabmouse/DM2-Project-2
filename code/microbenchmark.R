library(boot)
library(microbenchmark)
set.seed(5763)

bst <- function(formula, data, indices){
  d <- data[indices, ]
  fit <- lm(formula, data=d)
  return(coef(fit)) 
}

bootmodel <- lm (Oxygen~., data = fitness)

bootinterval <- function(n){
  result <- boot(data = fitness, statistic = bst, R = n, formula = bootmodel)
  return(result)
}

boot_microbenchmark <- microbenchmark(bootinterval, times = 10L)
lmboot_4_microbenchmark <- microbenchmark(lmBoot_4, times = 10L)

print(boot_microbenchmark)
print(lmboot_4_microbenchmark)

boxplot(boot_microbenchmark)
boxplot(lmboot_4_microbenchmark)