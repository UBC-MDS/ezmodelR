library(ezmodelR)


library(ggplot2)
library(caret)
library(rpart)
library(mlbench)
data(Sonar)

#Tests:

#Test A
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

  train_test_plot_instance <- train_test_plot(model = "decision_tree", score_type = "accuracy", x = Sonar[,1:60],
                                              y = Sonar[,61], hyperparameter = "cp",param_range = seq(0,1,.05), random_seed= 123)


  expect_identical(compare_plot$data, train_test_plot_instance$data)

})


#Test B

test_that("random_seed is numeric", {
  expect_warning(test_train_plot(model = "decision_tree" , score_type = "accuracy", x = Sonar[,1:60],
                                 y = Sonar[,61], hyperparameter = "cp",param_range = seq(0,1,.05), random_seed= NA), "random_seed needs to be numeric.")
})


#Test C
test_that("Function returns error when unexpected model is specified", {
  expect_warning(test_train_plot(model = "ridge" , score_type = "accuracy", x = Sonar[,1:60],
                                 y = Sonar[,61], hyperparameter = "cp", random_seed= 123), "'lasso', 'ridge', and 'logistic' regression are not implemented yet. Please, choose model = 'decision tree'")
})


#Test D
test_that("Function returns error when unexpected score is specified", {
  expect_warning(test_train_plot(model = "decision_tree" , score_type = NA, x = Sonar[,1:60],
                                 y = Sonar[,61], hyperparameter = "cp",param_range = seq(0,1,.05), random_seed= 123), "score_type for decision_tree needs to be 'accuracy'")
})


#Test E
test_that("Function returns error when unexpected hyperparameter is specified", {
  expect_warning(test_train_plot(model = "decision_tree" , score_type = "accuracy", x = Sonar[,1:60],
                                 y = Sonar[,61], hyperparameter = NA,param_range = seq(0,1,.05), random_seed= 123), "The hyperparameter for a decision_tree has to be 'cp'")
})





