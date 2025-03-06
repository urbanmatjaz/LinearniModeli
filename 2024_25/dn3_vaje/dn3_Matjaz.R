library(car)

getData = function(beta_3, n = 100, beta_0 = 10, beta_1 = 0.5, beta_2 = 0.15, sigma = 10) {
  set.seed(1) 
  x1 = runif(n, 50, 100)
  x2 = runif(n, 50, 100)
  
  eps = rnorm(n, mean = 0, sd = sigma)
  y = beta_0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x1 * x2 + eps
  
  data.frame(x1 = x1, x2 = x2, y = y)
}

beta_3_values = c(0, 0.05, 0.5)
data_list = lapply(beta_3_values, getData)


data_1 <- data_list[[1]]  
model_1 <- lm(y ~ x1 + x2, data = data_1)
data_2 <- data_list[[2]]  
model_2 <- lm(y ~ x1 + x2, data = data_2)
data_3 <- data_list[[3]]  
model_3 <- lm(y ~ x1 + x2, data = data_3)

par(mfrow = c(2, 2))

summary(model_1)
plot(model_1)
avPlots(model_1)
crPlots(model_1)

summary(model_2)
plot(model_2)
avPlots(model_2)
crPlots(model_2)

summary(model_3)
plot(model_3)
avPlots(model_3)
crPlots(model_3)
