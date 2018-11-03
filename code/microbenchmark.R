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
  a<-boot.ci(result, type = "basic", index = 1)
  b<-boot.ci(result, type = "basic", index = 2)
  c<-boot.ci(result, type = "basic", index = 3)
  d<-boot.ci(result, type = "basic", index = 4)
  e<-boot.ci(result, type = "basic", index = 5)
  f<-boot.ci(result, type = "basic", index = 6)
  g<-boot.ci(result, type = "basic", index = 7)
  return(c(a,b,c,d,e,f,g))
}

boot_microbenchmark <- microbenchmark(bootinterval, times = 10L)
lmboot_4_microbenchmark <- microbenchmark(lmBoot_4, times = 10L)

print(boot_microbenchmark)
print(lmboot_4_microbenchmark)

boxplot(boot_microbenchmark)
boxplot(lmboot_4_microbenchmark)