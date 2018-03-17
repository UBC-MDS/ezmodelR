# R Script file containing function definitions for the ezmodelR package.

library(ggplot2)
library(caret)
library(glmnet)
library(dplyr)
library(glue)
library(rpart)
library(mlbench)
data(Sonar)

train_test_plot <- function(model, score_type, x, y, hyperparameter, param_range, random_seed){

  #' Creates plot of training and test error for trained model.
  #'
  #' @param model: Currently only works with argument 'decision_tree'.
  #'         Generally:
  #'         String specifying decision_tree, lasso, ridge regression, or logistic regression.
  #'         Argument should be one of "decision_tree", "lasso", "ridge", or "logistic"..
  #' @param score_type: (list or str): Should be one of (mse, r2, adj_r2, auc, ...).
  #'               If a vector, then a vector containing several of those entries as elements
  #' @param x: n x d dataframe containing features
  #' @param y: n x 1 dataframe containing response values.
  #' @param hyperparameter: string defining hyperparameter to iterate over
  #' @param param_range: vector of hyperparameter values to iterate over
  #' @param random_seed: Default = None. If set to integer, defines the random train_test_split
  #' @return ggplot object showing training and test score vs. hyperparameter values.
  #' @examples
  #' train_test_plot(model = "decision_tree", score_type = "accuracy", x = Y,
  #`                 y = Y, hyperparameter = "cp", param_range = range(...), random_seed= ...)
  #'

  #Condition

  #True: A, False: B
  if(class(random_seed) == "numeric"){
    set.seed(random_seed)
  } else {
    stop("random_seed needs to be numeric.")
  }

  #Condition: C
  if (!(model == "decision_tree")){
    stop("'lasso', 'ridge', and 'logistic' regression are not implemented yet. Please, choose model = 'decision_tree'")
  }



  dat <- cbind(x,y)

  inTraining <- createDataPartition(dat$y, p = .75, list = FALSE)
  training <- dat[ inTraining,]
  testing  <- dat[-inTraining,]
  ctrl <- trainControl(method="none")

  train_acc_list <- c()
  test_acc_list <- c()
  index_list <- c()


  #Condition: D
  if (model == "decision_tree"){

    #Condition E
    if(!(score_type == "accuracy")){
      stop("score_type for decision_tree needs to be 'accuracy'")
    }

    #Condition: F
    if(!(hyperparameter == "cp")){
      stop("The hyperparameter for a decision_tree has to be 'cp'")
    }

    for (i in param_range){
      cp = hyperparameter
      params <- data.frame(cp=i)
      train_model <- train(y ~ ., data = training, method = 'rpart', trControl = ctrl, tuneGrid = params)

      train_pred <- predict(train_model, training, type = "raw")
      true_train <- training$y
      #call accuracy from Tyler
      train_acc <- sum(train_pred == true_train)/length(training$y)

      test_pred <- predict(train_model, testing, type = "raw")
      true_test <- testing$y
      #call accuracy from Tyler
      test_acc <- sum(test_pred == true_test)/length(testing$y)

      train_acc_list <- c(train_acc_list, train_acc)
      test_acc_list <- c(test_acc_list, test_acc)
      index_list <- c(index_list,i)
    }
  }

  results <- data.frame(cp = index_list, training_accuracy = train_acc_list, testing_accuracy = test_acc_list)

  train_test_plot <- ggplot(results)+
    geom_line(aes(x = cp, y = training_accuracy), color = "darkred", size = 1, alpha = 0.5)+
    geom_line(aes(x = cp, y = testing_accuracy), color = "darkblue", size = 1, alpha = 0.5)+
    ggtitle("Train Test Plot")+
    theme_bw()

  return(train_test_plot)
}


regularization_plot <- function(model, lambda, tol=1e-7, x, y){
  #' Plots coefficients from regularizing various models.
  #'
  #' @param model: String specifying lasso, ridge regression, or logistic regression with L2-regularization.
  #'         Argument should be one of "lasso", "ridge", or "logistic".
  #' @param lambda: Vector of penalty constant(s) multiplying the regularization term. Larger value corresponds to stronger regularization.
  #' @param  tol: Coefficients less than this will be treated as zero.
  #' @param  x:  n x d dataframe of features.
  #' @param  y: n x 1 dataframe of response values.
  #'
  #' @return ggplot object. Plot returned depends on length of lambda.
  #'     length(lambda)==1: Plot displays magnitude of model coefficients, where coefficients with magnitude less than
  #'                        `tol` are treated as zero.
  #'     length(lambda)>1: Plot displays counts of nonzero coefficients in each model, where coefficients with magnitude
  #'                       less than `tol` are treated as zero.
  #' @import dplyr
  #' @import caret
  #' @import ggplot2
  #' @export
  #'
  #' @examples
  #' X <- mtcars[-1]
  #' Y <- data.frame(mtcars$mpg)
  #'
  #' regularization_plot('ridge', lambda=2^c(1,0,1), x=X, y=Y)
  #' regularization_plot('lasso', lambda=2, x=X, y=Y)

  #True, A, False: B
  if (!(model %in% c('ridge', 'lasso', 'logistic'))) {
    stop("model specified must be one of 'ridge', 'lasso' or 'logistic'")
  }

  ctrl <- trainControl(method="none")

  # True: C, False: D
  if (model == 'logistic') {
    model_family <- "binomial"
  } else {
    model_family <- "gaussian"
  }

  #True: E, False: F
  if (model == "lasso") {
    model_alpha <- 1
  } else {
    model_alpha <- 0
  }

  N <- length(lambda)
  x$regplot_y <- y[[1]]

  #True: G, False: H
  if (N == 1) {
    params <- data.frame(alpha = model_alpha, lambda = lambda)
    mod <- train(regplot_y ~ ., data=x, method='glmnet', family=model_family, trControl=ctrl, tuneGrid=params)
    coefs <- abs(coef(mod$finalModel, mod$finalModel$lambdaOpt) %>% as.vector())
    coefs <- sapply(coefs, function(x) ifelse(abs(x) < tol, 0, x))

    df_coef <- data.frame(x=(1:length(coefs)), y=coefs)

    p <- df_coef %>%
      ggplot(aes(x=x, y=y)) +
      geom_line(colour="gray") +
      geom_point() +
      theme_bw() +
      labs(title="Magnitude of Model Coefficients", x="Magnitude", y="Coefficient")

  } else if (N > 1) {

    coef_mat <- matrix(ncol=2, nrow=N)
    colnames(coef_mat) <- c("lambda", "nonzero_count")

    for (i in 1:N) {
      params <- data.frame(alpha=model_alpha, lambda=lambda[i])
      mod <- train(regplot_y ~ ., data=x, method="glmnet", family=model_family, trControl=ctrl, tuneGrid=params)
      coefs <- coef(mod$finalModel, mod$finalModel$lambdaOpt) %>% as.vector()
      coef_mat[i,] <- c(lambda[i], sum(abs(coefs) >= tol))
    }

    df_coef <- as.data.frame(coef_mat)

    p <- df_coef %>%
      ggplot(aes(x=lambda, y=nonzero_count)) +
      geom_line(colour="gray") +
      geom_point() +
      theme_bw() +
      labs(title="Number of Nonzero Coefficients vs. Regularization Strength (lambda)", x="lambda", y="Number of Nonzero Coefficients") +
      scale_x_continuous(breaks=df_coef$lambda, labels=df_coef$lambda)
  }

  return(p)

}


score <- function(model, score_type, train_settings=trainControl(method='none')) {
  #' Used to compute an arbitrary score method on arbitrary inputs.
  #'
  #' @param model (char): Model type that can be passed into caret's `train()` function.
  #' @param score_type (char): String specifying score method to be used. Should be one of (mse, accuracy, r2, adj_r2, auc, ...).
  #' @param train_settings (list): default=trainControl(method='none'). trainControl object containing any desired settings for caret's train() function.
  #' @return Function that takes dataframes X (features, n x d) and Y (response, n x 1) that score will be computed on.
  #'
  #' @import dplyr
  #' @import caret
  #' @export
  #'
  #' @examples
  #' rf_sens_func <- score('rf','sensitivity')


  mse <- function(x,y){
    model <- train(x, as.factor(y), method=model, trControl=train_settings
    ) # Note the as.numeric(as.factor()) magic, Currently only suports Classification
    y_pred <- predict(model)

    score <- sum(((as.numeric(y_pred) - as.numeric(as.factor(y)))^2))
    return (score)
  }

  accuracy <- function(x, y){
    model <- train(x, as.factor(y), method=model, trControl=train_settings)
    y_pred <- predict(model)

    score <- mean(as.numeric(y_pred) == as.numeric(as.factor(y)))
    return(score)
  }

  specificity <- function(x,y){
    model <- train(x, as.factor(y), method=model, trControl=train_settings)
    y_pred <- predict(model)


    trueneg <- function(y_true, y_pred){
      num_tn <- 0
      for(i in 1:length(y_true)){
        if(all(c(y_true[i] == 0, y_pred[i] == 0))) {
          num_tn <- num_tn + 1
        }
      }
      return(num_tn)
    }

    falsepos <- function(y_true, y_pred){
      num_fp <- 0
      for(i in 1:length(y_true)){
        if(all(c(y_true[i] == 0, y_pred[i] == 1))) {
          num_fp <- num_fp + 1
        }
      }
      return(num_fp)
    }

    score <- trueneg(y, y_pred)/(trueneg(y, y_pred) + falsepos(y, y_pred))

    return(score)
  }

  sensitivity <- function(x,y){

    model <- train(x, as.factor(y), method=model, trControl=train_settings)
    y_pred <- predict(model)


    truepos <- function(y_true, y_pred){
      num_tp <- 0
      for(i in 1:length(y_true)){
        if(all(c(y_true[i] == 1, y_pred[i] == 1))) {
          num_tp <- num_tp + 1
        }
      }
      return(num_tp)
    }

    falseneg <- function(y_true, y_pred){
      num_fn <- 0
      for(i in 1:length(y_true)){
        if(all(c(y_true[i] == 1, y_pred[i] == 0))) {
          num_fn <- num_fn + 1
        }
      }
      return(num_fn)
    }

    score <- truepos(y, y_pred)/(truepos(y, y_pred) + falseneg(y, y_pred))

    return(score)
  }

  r2 <- function(x, y){
    model <- train(x, as.factor(y), method=model, trControl=train_settings)
    y_pred <- predict(model)

    score <- 1 - ((sum((as.numeric(as.factor(y)) - as.numeric(as.factor(y_pred)))**2))/(sum((y - mean(y))**2)))
    return(score)
  }

  adj_r2 <- function(x, y){

    n <- dim(x)[1]
    p <- dim(x)[2]

    model <- train(x, as.factor(y), method=model, trControl=train_settings)
    y_pred <- predict(model)

    score <- 1 - (1 - (r2(y, y_pred)*((n - 1)/(n - p - 1))))
    return(score)
  }

  supported <- c(mse, accuracy, r2, adj_r2, sensitivity, specificity)
  names(supported) <- c('mse', 'accuracy', 'r2','adj_r2','sensitivity','specificity')

  if(score_type %in% names(supported)){ # A
    return(supported[[score_type]])
  }
  else{ # B
    stop(print(glue("{score_type} is not currently supported.")))
  }
}
