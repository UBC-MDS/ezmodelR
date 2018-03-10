# R Script file containing function definitions for the ezmodelR package.

library(ggplot2)
library(caret)
library(glmnet)
library(dplyr)

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

