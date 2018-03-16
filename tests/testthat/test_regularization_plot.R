library(ezmodelR)


test_that("Function returns error when wrong type of model is specified", {
  X <- mtcars[-1]
  Y <- mtcars[1]

  expect_error(regularization_plot("lm",lambda=2, x=X, y=Y), "model specified must be one of 'ridge', 'lasso' or 'logistic'")

})

test_that("Coefficient counts being plotted are correct", {

  # matrix of coefficients
  coef_mat <- matrix(ncol=2, nrow=5)
  colnames(coef_mat) <- c("lambda", "nonzero_count")

  # params for the models being built
  ctrl <- trainControl(method="none")
  tol=1e-7

  # create lasso model in caret for each lambda in 2^c(-2,...,2)
  for (i in 1:5) {
    params <- data.frame(alpha=1, lambda=2^(i-3))
    lasso_mod <- train(mpg ~ ., data=mtcars, method="glmnet", family="gaussian", trControl=ctrl, tuneGrid=params)
    coefs <- coef(lasso_mod$finalModel, lasso_mod$finalModel$lambdaOpt) %>% as.vector()
    coef_mat[i,] <- c(2^(i-3), sum(abs(coefs) >= tol))  # set coefs below tolerance to 0
  }

  df_coef <- as.data.frame(coef_mat)

  # manually create ggplot object from coefficients
  p <- df_coef %>%
    ggplot(aes(x=lambda, y=nonzero_count)) +
    geom_line(colour="gray") +
    geom_point() +
    theme_bw() +
    labs(title="Number of Nonzero Coefficients vs. Regularization Strength (lambda)", x="lambda", y="Number of Nonzero Coefficients") +
    scale_x_continuous(breaks=df_coef$lambda, labels=df_coef$lambda)

  # format data for reg_plot function
  X <- mtcars[-1]
  Y <- data.frame(mtcars$mpg)
  reg_p <- regularization_plot("lasso",lambda=2^c(-2,-1,0,1,2), x=X, y=Y)

  expect_identical(p$data, reg_p$data)

})


test_that("Function handles logistic models corectly", {

  # params for models
  ctrl <- trainControl(method="none")
  tol <- 1e-7

  # format data for function
  dat <- ChickWeight %>%
    as.data.frame() %>%
    filter(Diet %in% c(1,2)) %>%
    droplevels() %>%
    select(-Chick)

  # create logistic regression model in caret with specified params
  params <- data.frame(alpha=0, lambda=2)
  log_mod <- train(Diet ~ ., data=dat, method="glmnet", family="binomial", trControl=ctrl, tuneGrid=params)
  coefs <- abs(coef(log_mod$finalModel, log_mod$finalModel$lambdaOpt) %>% as.vector())
  coefs <- sapply(coefs, function(x) ifelse(abs(x) < tol, 0, x))  # set coefs below tolerance to 0

  df_coef <- data.frame(x=(1:length(coefs)), y=coefs)

  # manually create ggplot object
  p <- df_coef %>%
    ggplot(aes(x=x, y=y)) +
    geom_line(colour="gray") +
    geom_point() +
    theme_bw() +
    labs(title="Magnitude of Model Coefficients", x="Coefficient", y="Magnitude")

  # split data for reg_plot
  X <- dat %>%
    select(-Diet)
  Y <- data.frame(dat$Diet)
  reg_p <- regularization_plot('logistic',lambda=2, x=X, y=Y)

  expect_identical(p$data, reg_p$data)

})

test_that("Passing a single argument for lambda plots correct coefficients", {

  # params for models
  ctrl <- trainControl(method="none")
  tol <- 1e-7

  # create single model with specified params
  params <- data.frame(alpha=0, lambda=2)  # ridge is specified by alpha=0
  lasso_mod <- train(mpg ~ ., data=mtcars, method="glmnet", family="gaussian", trControl=ctrl, tuneGrid=params)
  coefs <- abs(coef(lasso_mod$finalModel, lasso_mod$finalModel$lambdaOpt) %>% as.vector())
  coefs <- sapply(coefs, function(x) ifelse(abs(x) < tol, 0, x))

  df_coef <- data.frame(x=(1:length(coefs)), y=coefs)

  # manually create ggplot object
  p <- df_coef %>%
    ggplot(aes(x=x, y=y)) +
    geom_line(colour="gray") +
    geom_point() +
    theme_bw() +
    labs(title="Magnitude of Model Coefficients", x="Coefficient", y="Magnitude")

  X <- mtcars[-1]
  Y <- data.frame(mtcars$mpg)
  reg_p <- regularization_plot("ridge",lambda=2, x=X, y=Y)

  expect_identical(p$data, reg_p$data)

})
