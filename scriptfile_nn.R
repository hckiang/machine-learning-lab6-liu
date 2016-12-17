# ----------------------------------------
# Introduction to Machine Learning
# (732A95)
#
# Lab 6, block 1
# ----------------------------------------

# Packages -------------------------------

library(knitr) 
library(ggplot2) 
library(neuralnet)


set.seed(1234567890)

n <- 50
xVar <- runif(n,0,10)
data_original <- data.frame(x = xVar,
                            sin = sin(xVar))

split_idx <- 1:(n*0.5)
data_train <- data_original[split_idx,]
data_val <- data_original[-split_idx,]

weightInit <- runif(n,-1,1)



bestThreshold <- lapply(seq_along(1:10), function(i){
  
  nn_temp <- neuralnet(sin ~ x, 
                       data = data_train,
                       hidden = 10,
                       threshold = i/1000,
                       startweights = weightInit) 
  
  pred_trainTemp <- compute(nn_temp, data_train$x)
  pred_valTemp <- compute(nn_temp, data_val$x)
  
  mse_trainTemp <- mean((data_train$sin - pred_trainTemp$net.result)^2)
  mse_valTemp <- mean((data_val$sin - pred_valTemp$net.result)^2)
  
  return(list("MSE_Train" = mse_trainTemp,
              "MSE_Val" = mse_valTemp,
              "Threshold" = i/1000))
})

mse_Train <- vapply(bestThreshold, function(x){ x[["MSE_Train"]]}, numeric(1))
mse_val <- vapply(bestThreshold, function(x){ x[["MSE_Val"]]}, numeric(1))
threshold <- vapply(bestThreshold, function(x){ x[["Threshold"]]}, numeric(1))

#par(mfrow = c(1,2))
#plot(x = 1:10, y = mse_Train, type = "l", 
#     ylab = "MSE", xlab = "Index", main = "Train")
plot(x = 1:10, y = mse_val, type = "l",
     ylab = "MSE", xlab = "Index", main = "Validation")
#par(mfrow = c(1,1))

optimalThreshold <- threshold[which.min(mse_val)]

nn_final <- neuralnet(sin ~ x, 
                      data = data_original,
                      hidden = 10,
                      threshold = optimalThreshold,
                      startweights = weightInit) 

finalPred <- prediction(nn_final)$rep1

plot(x = finalPred[,1], y = finalPred[,2], 
     col = "black", xlab = "X", ylab = expression(sin(x)),
     pch = 16)
points(x = data_original$x, y = data_original$sin,
       pch = 16, col = "red")