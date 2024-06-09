df = read.csv("C:\\Users\\Konstantin\\Documents\\data.csv")
dfs= df[sample(1:nrow(df)), ] 
library(frbs)

data(dfs)

data.train <- dfs[1 : 5000, ]

data.fit <- data.train[, 1 : 5]

data.tst <- dfs[5000:9999, 1 : 5]

real.val <- matrix(dfs[500:9999, 6], ncol = 1)

##range.data<-apply(data.train, 5, range)

method.type <- "ANFIS"

control <- list(num.labels = 6, max.iter = 100, step.size = 0.01, type.tnorm = "MIN", type.snorm = "MAX", type.implication.func = "ZADEH", name = "Tows")

## generate fuzzy model
object <- frbs.learn(data.train, NULL, method.type, control)

## This process is a part of fitting the model using data training. 
res.fit <- predict(object, data.fit)

## predicting step
res.test <- predict(object, data.tst)

## error calculation
y.pred <- res.test

y.real <- real.val

bench <- cbind(y.pred, y.real)

colnames(bench) <- c("pred. val.", "real. val.")

print("Comparison ANFIS Vs Real Value on Data Set")
[1] "Comparison ANFIS Vs Real Value on Data Set"

print(bench)
residuals <- (y.real - y.pred)

MSE <- mean(residuals^2)

RMSE <- sqrt(mean(residuals^2))

SMAPE <- mean(abs(residuals)/(abs(y.real) + abs(y.pred))/2)*100

err <- c(MSE, RMSE, SMAPE)

names(err) <- c("MSE", "RMSE", "SMAPE")

print("Error Measurement: ")
[1] "Error Measurement: "

print(err)  

op <- par(mfrow = c(2, 1))

x1 <- seq(from = 1, to = nrow(res.fit))

result.fit <- cbind(data.train[, 3], res.fit)

plot(x1, result.fit[, 1], col="red", main = "Data: Fitting phase (the training data(red) Vs Sim. result(blue))", type = "l", ylab = "CO2")

lines(x1, result.fit[, 2], col="blue")

result.test <- cbind(real.val, res.test)

x2 <- seq(from = 1, to = nrow(result.test))

plot(x2, result.test[, 1], col="red", main = "Data: Predicting phase (the Real Data(red) Vs Sim. result(blue))", type = "l", ylab = "CO2")

lines(x2, result.test[, 2], col="blue", type = "l")

par(op)
