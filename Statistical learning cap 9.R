# linear classifier ----
set.seed (1)
x <- matrix(rnorm (20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1, ] <- x[y == 1, ] + 1
plot(x, col = (3 - y))


dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y ~ ., data = dat , kernel = "linear",
              cost = 10, scale = FALSE)
plot(svmfit, dat)


summary(svmfit)


tune.out <- tune(svm , y ~ ., data = dat , kernel = "linear",
                 ranges = list(cost = c(0.001 , 0.01, 0.1, 1, 5, 10, 100)))

summary(tune.out)

tune.out$best.model


## nonlinear kernel----

set.seed (1)
x <- matrix(rnorm (200 * 2), ncol = 2)
x[1:100 , ] <- x[1:100 , ] + 2
x[101:150 , ] <- x[101:150 , ] - 2
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))

plot(x, col = y)

# radial kernel, gamma = 1 and cost = 1 ----
# the bigger the gamma constant, the more flexible the decision boundary
# observations far from the support vectors have a smaller impact on the decision boundary
train <- sample (200, 100)
svmfit <- svm(y ~ ., data = dat[train , ], kernel = "radial",
                gamma = 1, cost = 1)
plot(svmfit , dat[train , ])

# training and test
train <- sample (200, 100)
svmfit <- svm(y ~ ., data = dat[train , ], kernel = "radial",
                gamma = 1, cost = 1)
plot(svmfit , dat[train , ])

summary(svmfit)

#increasing cost, risk of overfitting
svmfit <- svm(y ~ ., data = dat[train , ], kernel = "radial",
              gamma = 1, cost = 1e5)
plot(svmfit , dat[train , ])

# 10-fold cross validation
set.seed (1)
tune.out <- tune(svm , y ~ ., data = dat[train , ],
                   kernel = "radial",
                   ranges = list(
                     cost = c(0.1, 1, 10, 100, 1000) ,
                     gamma = c(0.5, 1, 2, 3, 4)
                   )
)
summary(tune.out)


# performance of best model

table(
  true = dat[-train , "y"],
  pred = predict(
    tune.out$best.model , newdata = dat[-train , ]
  )
)


# ROC curves ----

# We first write a short function to plot an ROC curve
# given a vector containing a numerical score for each observation, pred, and
# a vector containing the class label for each observation, truth.

rocplot <- function(pred , truth , ...) {
   predob <- prediction(pred , truth)
   perf <- performance(predob , "tpr", "fpr")
   plot(perf , ...)
}


# In essence, the sign of the fitted value determines
# on which side of the decision boundary the observation lies. Therefore, the
# relationship between the fitted value and the class prediction for a given
# observation is simple: if the fitted value exceeds zero then the observation
# is assigned to one class, and if it is less than zero then it is assigned to the
# other.

# fitted values (decision values = T)

svmfit.opt <- svm(y ~ ., data = dat[train , ],
                  kernel = "radial", gamma = 2, cost = 1,
                  decision.values = T)
fitted <- attributes(
  predict(svmfit.opt , dat[train , ], decision.values = TRUE)
)$decision.values

# Note we use the negative of the fitted
# values so that negative values correspond to class 1 and positive values to
# class 2

par(mfrow = c(1, 2))
rocplot(-fitted , dat[train , "y"], main = "Training Data")

# SVM appears to be producing accurate predictions. By increasing γ we can
# produce a more flexible fit and generate further improvements in accuracy.
svmfit.flex <- svm(y ~ ., data = dat[train , ],
                     kernel = "radial", gamma = 50, cost = 1,
                     decision.values = T)

fitted <- attributes(
  predict(svmfit.flex , dat[train , ], decision.values = T)
)$decision.values

rocplot(-fitted , dat[train , "y"], add = T, col = "red")

# now the performance on the test data

fitted <- attributes(
  predict(svmfit.opt , dat[-train , ], decision.values = T)
)$decision.values
# in black, better performance, gamma = 1, cost = 1
rocplot(-fitted , dat[-train , "y"], main = "Test Data")

fitted <- attributes(
  predict(svmfit.flex , dat[-train , ], decision.values = T)
)$decision.values
# super flexible in red, gamma 50, cost 1
rocplot(-fitted , dat[-train , "y"], add = T, col = "red")