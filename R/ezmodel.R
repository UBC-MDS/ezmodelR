# R Script file containing function definitions for the ezmodelR package.

library(ggplot2)
library(caret)
library(glmnet)
library(dplyr)
library(glue)

train_test_plot <- function(x){
  # Description:
  #   Creates plot of training and test error for trained model.
  #
  # Args:
  #   model: Trained model that can be passed into caret's `predict` function.
  #   score_type: (list or str): Should be one of (mse, r2, adj_r2, auc, ...).
  #               If a vector, then a vector containing several of those entries as elements
  #   X: n x d dataframe containing features
  #   Y: n x 1 dataframe containing response values.
  #   hyperparameter: vector of hyperparameter values to iterate over.
  #
  # Returns:
  #   ggplot object showing training and test score vs. hyperparameter values.
  #
  return(x)
}


regularization_plot <- function(model, lambda, tol=1e-7, x, y){
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

  if (!(model %in% c('ridge', 'lasso', 'logistic'))) {
    stop("model specified must be one of 'ridge', 'lasso' or 'logistic'")
  }

  ctrl <- trainControl(method="none")

  if (model == 'logistic') {
    model_family <- "binomial"
  } else {
    model_family <- "gaussian"
  }

  if (model == "lasso") {
    model_alpha <- 1
  } else {
    model_alpha <- 0
  }

  N <- length(lambda)
  x$regplot_y <- y[[1]]

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
  # Description:
  #   Used to compute an arbitrary score method on arbitrary inputs.
  #
  # Args:
  #   model (char): Model type that can be passed into caret's `train()` function.
  #   score_type (char): String specifying score method to be used. Should be one of (mse, accuracy, r2, adj_r2, auc, ...).
  #   train_settings (list): default=trainControl(method='none'). trainControl object containing any desired settings for caret's                                    train() function.
  #
  # Returns:
  #   Function that takes dataframes X (features, n x d) and Y (response, n x 1) that score will be computed on.


  mse <- function(x,y){
    model <- train(x, as.factor(y), method=model, trControl=train_settings
    ) # Note the as.numeric(as.factor()) magic, Currently only suports Classification
    y_pred <- predict(model)

    score <- sum(((as.numeric(y_pred) - as.numeric(as.factor(y))^2)))
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

    # Something not working here. Unsure what.

    n <- dim(x)[1]
    p <- dim(x)[2]

    model <- train(x, as.factor(y), method=model, trControl=train_settings)
    y_pred <- predict(model)

    score <- 1 - (1 - (r2(y, y_pred)*((n - 1)/(n - p - 1))))
    return(score)
  }

  supported <- c(mse, accuracy, r2, adj_r2, sensitivity, specificity)
  names(supported) <- c('mse', 'accuracy', 'r2','adj_r2','sensitivity','specificity')

  if(score_type %in% names(supported)){
    return(supported[[score_type]])
  }
  else{
    stop(print(glue("{score_type} is not currently supported.")))
  }
}
