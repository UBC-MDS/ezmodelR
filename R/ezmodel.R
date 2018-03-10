# R Script file containing function definitions for the ezmodelR package.


train_test_plot <- function(model, score_type, X, Y, hyperparameter, param_range, random_seed){
  # Description:
  #   Creates plot of training and test error for trained model.
  #
  # Args:
  #   model: Trained model that can be passed into caret's `predict` function.
  #   score_type: (list or str): Should be one of (mse, r2, adj_r2, auc, ...).
  #               If a vector, then a vector containing several of those entries as elements
  #   X: n x d dataframe containing features
  #   Y: n x 1 dataframe containing response values.
  #   hyperparameter: string defining hyperparameter to iterate over
  #   param_range: vector of hyperparameter values to iterate over
  #   random_seed: Default = None. If set to integer, defines the random train_test_split
  #
  # Returns:
  #   ggplot object showing training and test score vs. hyperparameter values.

  library(ggplot2)
  library(caret)

  if(class(random_seed) == "numeric"){
    set.seed(random_seed)
  } else {
    stop("random_seed needs to be numeric.")
  }

  dat <- cbind(X,Y)

  inTraining <- createDataPartition(dat$Y, p = .75, list = FALSE)
  training <- dat[ inTraining,]
  testing  <- dat[-inTraining,]
  ctrl <- trainControl(method="none")

  train_acc_list <- c()
  test_acc_list <- c()
  index_list <- c()


  for (i in param_range){
    cp = hyperparameter
    params <- data.frame(cp=i)
    train_model <- train(Y ~ ., data = training, method = model, trControl = ctrl, tuneGrid = params)

    train_pred <- predict(train_model, training, type = "raw")
    true_train <- training$Y
    #call accuracy from Tyler
    train_acc <- sum(train_pred == true_train)/length(training$Y)

    test_pred <- predict(train_model, testing, type = "raw")
    true_test <- testing$Y
    #call accuracy from Tyler
    test_acc <- sum(test_pred == true_test)/length(testing$Y)

    train_acc_list <- c(train_acc_list, train_acc)
    test_acc_list <- c(test_acc_list, test_acc)
    index_list <- c(index_list,i)
  }

  results <- data.frame(hyperparameter = index_list, training_accuracy = train_acc_list, testing_accuracy = test_acc_list)

  train_test_plot <- ggplot(results)+
    geom_line(aes(x = hyperparameter, y = training_accuracy), color = "darkred", size = 1, alpha = 0.5)+
    geom_line(aes(x = hyperparameter, y = testing_accuracy), color = "darkblue", size = 1, alpha = 0.5)+
    ggtitle("Train Test Plot")+
    theme_bw()

  print(train_acc_list)
  print(test_acc_list)
  print(index_list)


  return(train_test_plot)
}

train_test_plot(model = "rpart", score_type = "accuracy", X = Sonar[,1:60],
                Y = Sonar[,61], hyperparameter = "cp" ,param_range = c(0.2,0.3,0.4,0.5), random_seed= 123)



regularization_plot <- function(model,lambda,tol=1e-7,x,y){
  # Description:
  #   Plots coefficients from regularizing various models.
  #
  # Args:
  #   model: String specifying lasso, ridge regression, or logistic regression with L2-regularization.
  #         Argument should be one of "lasso", "ridge", or "logistic".
  #   lambda: Vector of penalty constant(s) multiplying the regularization term. Larger value corresponds to stronger regularization.
  #   tol: Coefficients less than this will be treated as zero.
  #   x:  n x d dataframe of features.
  #   y: n x 1 dataframe of response values.
  #
  #
  # Returns:
  #   ggplot object. Plot returned depends on length of lambda.
  #     length(lambda)==1: Plot displays magnitude of model coefficients, where coefficients with magnitude less than
  #                        `tol` are treated as zero.
  #     length(lambda)>1: Plot displays counts of nonzero coefficients in each model, where coefficients with magnitude
  #                       less than `tol` are treated as zero.
  #
  return(x)
}


score <- function(model, score_type, train_settings=trainControl(method='none')) {
  # Description:
  #   Used to compute an arbitrary score method on arbitrary inputs.
  #
  # Args:
  #   model (char): Model type that can be passed into caret's `train()` function.
  #   score_type (char): String specifying score method to be used. Should be one of (mse, accuracy, r2, adj_r2, auc, ...).
  #   train_settings (list): default=trainControl(method='none'). trainControl object containing any desired settings for caret's train() function.
  #
  # Returns:
  #   Function that takes dataframes X (features, n x d) and Y (response, n x 1) that score will be computed on.

  return(NULL)
}

