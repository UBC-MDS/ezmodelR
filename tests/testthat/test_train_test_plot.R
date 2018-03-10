train_test_plot <- function(x){
  # Description:
  #   Creates plot of training and test error for trained model.
  #
  # Args:
  #   model: String specifying model to evaluate. May be restricted to certain model-types in the process
  #          project
  #   score_type: (list or str): Should be one of (mse, r2, adj_r2, auc, accuracy ...).
  #               If a vector, then a vector containing several of those entries as elements
  #   X: n x d dataframe containing features
  #   Y: n x 1 dataframe containing response values.
  #   hyperparameter: vector of hyperparameter values to iterate over.
  #   random_seed: Default = None. If set to integer, defines the random train_test_split
  #
  #
  # Returns:
  #   ggplot object showing training and test score vs. hyperparameter values.
  #
  return(x)
}

#Tests:

library(ggplot2)
library(caret)
library(rpart)
library(mlbench)

data(Sonar)


test_that("Passing model, score_type, and hyperparameter leads to correct plot",{

  set.seed(123)

  inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)
  training <- Sonar[ inTraining,]
  testing  <- Sonar[-inTraining,]
  ctrl <- trainControl(method="none")

  train_acc_list <- c()
  test_acc_list <- c()
  cp_list <- c()

  for (i in seq(0,1,.05)){

    params <- data.frame(cp=i)
    tree_mod <- train(Class ~ ., data = training, method = "rpart", trControl = ctrl, tuneGrid = params)

    train_pred <- predict(tree_mod, training, type = "raw")
    true_train <- training$Class
    train_acc <- sum(train_pred == true_train)/length(training$Class)

    test_pred <- predict(tree_mod, testing, type = "raw")
    true_test <- testing$Class
    test_acc <- sum(test_pred == true_test)/length(testing$Class)

    train_acc_list <- c(train_acc_list, train_acc)
    test_acc_list <- c(test_acc_list, test_acc)
    cp_list <- c(cp_list,i)
  }

  results <- data.frame(cp = cp_list, training_accuracy = train_acc_list, testing_accuracy = test_acc_list)

  compare_plot <- ggplot(results)+
    geom_line(aes(x = cp, y = training_accuracy), color = "darkred", size = 1, alpha = 0.5)+
    geom_line(aes(x = cp, y = testing_accuracy), color = "darkblue", size = 1, alpha = 0.5)+
    ggtitle("Train Test Plot")+
    theme_bw()

  train_test_plot_instance <- test_train_plot(model = "rpart", score_type = "accuracy", X = Sonar[,1:60],
                                              Y = Sonar[,61], hyperparameter = "cp", random_seed= 123)


  expect_identical(compare_plot$data, train_test_plot_instance$data)

})




test_that("Function returns error when no/unexpected model is specified" {
  X <- mtcars[-1]
  Y <- mtcars[1]
  expect_warning(test_train_plot(model = NA , score_type = "accuracy", X = Sonar[,1:60],
                                 Y = Sonar[,61], hyperparameter = "cp", random_seed= 123), "specified model is not valid")
})



test_that("Function returns error when no/unexpected score is specified" {
  X <- mtcars[-1]
  Y <- mtcars[1]
  expect_warning(test_train_plot(model = "rpart" , score_type = NA, X = Sonar[,1:60],
                                 Y = Sonar[,61], hyperparameter = "cp", random_seed= 123), "specified score is not valid")
})



test_that("Function returns error when no/unexpected hyperparameter is specified" {
  X <- mtcars[-1]
  Y <- mtcars[1]
  expect_warning(test_train_plot(model = "rpart" , score_type = "accuracy", X = Sonar[,1:60],
                                 Y = Sonar[,61], hyperparameter = NA , random_seed= 123), "specified score is not valid")
})





