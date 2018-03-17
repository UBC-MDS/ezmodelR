library(dplyr)
library(caret)
library(ezmodelR)

test_that("score() returns a function", {
  expect_type(score("rf", "mse"), "closure")
})


test_that("score() using mse returns a double", {
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


test_that("score() using MSE on a random forest is working correctly", {
  # This covers case A in the score function
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

  y_pred <- predict(rf_model)
  expect_equal(score("rf","mse")(x,y), sum(((as.numeric(y_pred) - 1) - y)^2))
})

test_that("score() correctly returns an error when an unsupported score_type is passed", {
  # This covers case B in the score function
  expect_error(score('rf', 'banana'))

})


test_that("score() correctly returns an error when an unsupported model is passed", {
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

  expect_error(score('banana', "mse")(x,y))

})
