# R Script file containing function definitions for the ezmodelR package.


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


score <- function(model, score_type) {
  # Description:
  #   Used to compute an arbitrary score method on arbitrary inputs.
  #
  # Args:
  #   model: Trained model that can be passed into caret's `train()` function.
  #   score_type: String specifying score method to be used. Should be one of (mse, r2, adj_r2, auc, ...).
  #
  # Returns:
  #   Function that takes dataframes X (features, n x d) and Y (response, n x 1) that score will be computed on.

  return(NULL)
}

