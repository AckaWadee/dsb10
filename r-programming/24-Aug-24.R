### 24 Aug 2024 >> ML for DA part 1

library(tidyverse)
library(caret) # classification and regression tree

## load data
mtcars

## split data 70:30
# we need the balance data
set.seed(42)
n <- nrow(mtcars) ##32 records
id <- sample(1:n, size = 0.7*n)

train_df <- mtcars[id,]
test_df <- mtcars[-id,]

## train 
set.seed(42)
lm_model <- train(mpg ~ hp + wt + am,
                  data = train_df,
                  method = "lm")

## knn : k-nearest neighbors model
knn_model <- train(mpg ~ hp + wt + am,
                  data = train_df,
                  method = "knn")
lm_model

knn_model

lm_model$resample
## score
p_test <- predict(lm_model, newdata= test_df)
p_test_knn <- predict(knn_model, newdata = test_df)

## evaluate
error <- test_df$mpg - p_test
mae <- mean(abs(error))
mse <- mean(error**2)
rmse <- sqrt(mean(error**2))
# we can use these metrics to compare with another linear model
# low error is good model

## evaluate of knn
error_knn <- test_df$mpg - p_test_knn
mae_knn <- mean(abs(error_knn))
mse_knn <- mean(error_knn**2)
rmse_knn <- sqrt(mean(error_knn**2))

list(mae, mse, rmse)
list(mae_knn, mse_knn, rmse_knn)

### Euclidien Distance
x1 = c(2,3)
x2 = c(6,8)

dst = sqrt(sum((x1-x2)**2))

dst

#### test if we use all features in the dataset
set.seed(42)
lm_model <- train(mpg ~ .,
                  data = train_df,
                  method = "lm")

## knn : k-nearest neighbors model
knn_model <- train(mpg ~ .,
                   data = train_df,
                   method = "knn")

mae
mae_knn
## after use all feature in dataset >> the performance was reduced 
## focus on the quality of data/feature before trian
## out perform model >> โมเดลยิ่งง่าย ยิ่งดี, การเพิ่มตัวแปรที่ไม่ดีเข้าไปส่งผลกับโมเดล
# first model is the best


