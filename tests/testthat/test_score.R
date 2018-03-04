library(dplyr)
library(caret)

test_that("score() returns a function" {
  expect_type(score("rf", "mse"), "closure")
})

test_that("score() returns an error if a supported model type is not passed"{

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



