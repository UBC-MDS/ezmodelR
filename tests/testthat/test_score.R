library(dplyr)
library(caret)

test_that("score() returns a function" {
  expect_type(score("rf", "mse"), "closure")
})

test_that("score() returns an error if a supported model type is not passed"{
  expect_warning(score("hello",'mse'), "Model specified must be one supported by the 'caret' package")
})

test_that("score() returns an error if a supported score type is not passed"{
  expect_warning(score("rf",'goodbye'), "Score type specified must be one of 'mse', 'accuracy', 'r2', 'adj_r2', or 'roc'")
})

test_that("score() using mse returns a double"{
  is_setosa <- function(x){
    if(x == "setosa"){
      return(1)
    }
    else{
      return(0)
    }
  }

  y <-  apply(iris['Species'], 1, is_setosa)
  x <- iris %>% select("Sepal.Width")

  expect_type(score("rf","mse")(x,y), "double")
})


test_that("score() using MSE on a random forest is working correctly"{
  is_setosa <- function(x){
    if(x == "setosa"){
      return(1)
    }
    else{
      return(0)
    }
  }

  y <-  apply(iris['Species'], 1, is_setosa)
  x <- iris %>% select("Sepal.Width")

  settings <- trainControl(method='none')
  rf_model <- train(x, as.factor(y), method='rf', trControl = settings)

  y_pred <- predict(test)
  expect_equal(score("rf","mse")(x,y), sum(((as.numeric(y_pred) - 1) - y)^2))
})


