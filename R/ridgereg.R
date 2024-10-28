#' @title Ridge Regression
#' @description Perform ridge regression with L2 regularization.
#' @param formula A formula object, e.g., y ~ x1 + x2.
#' @param data A data frame containing the variables in the model.
#' @param lambda A numeric value specifying the ridge penalty parameter λ.
#' @return A list of class `ridgereg` containing ridge regression results.
#' @details This function fits a ridge regression model, regularizing the coefficients to prevent overfitting.
#'          It is particularly useful in cases of multicollinearity or when p > n (more predictors than observations).
#' @examples
#' model <- ridgereg(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris, lambda = 0.1)
#' model$summary()
#' @export
ridgereg <- function(formula, data, lambda) {
  
  # Design matrix X (independent variables) and response vector y (dependent variable)
  X <- model.matrix(formula, data)
  y <- data[[all.vars(formula)[1]]]
  
  # Normalize the predictor matrix X (excluding intercept if present)
  X_norm <- scale(X, center = TRUE, scale = TRUE)
  
  # Add λ to the diagonal of X'X to create the ridge regression solution
  lambda_I <- diag(lambda, ncol(X_norm))
  # Ensuring intercept term is not penalized by setting first element of lambda_I to zero
  lambda_I[1, 1] <- 0
  
  # Calculate ridge regression coefficients: βˆridge = (X'X + λI)^(-1) X'y
  beta_ridge <- solve(t(X_norm) %*% X_norm + lambda_I) %*% t(X_norm) %*% y
  
  # Calculate fitted values and residuals
  fitted_values <- X_norm %*% beta_ridge
  residuals <- y - fitted_values
  
  # Create the ridgereg object as an S3 list
  model <- list(
    formula = formula,
    data = data,
    lambda = lambda,
    coefficients = beta_ridge,
    fitted_values = fitted_values,
    residuals = residuals
  )
  class(model) <- "ridgereg"
  return(model)
}

#' @export
summary.ridgereg <- function(object) {
  cat("Ridge Regression Model\n")
  cat("Formula:", deparse(object$formula), "\n")
  cat("Lambda:", object$lambda, "\n\n")
  
  # Print coefficient estimates
  cat("Coefficients:\n")
  print(object$coefficients)
}

#' @export
predict.ridgereg <- function(object, newdata) {
  # Ensure the new data is normalized in the same way as the original data
  X_new <- model.matrix(object$formula, newdata)
  X_new_norm <- scale(X_new, center = TRUE, scale = TRUE)
  return(X_new_norm %*% object$coefficients)
}

