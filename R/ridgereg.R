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
#' print(model)
#' predict(model, newdata = iris)
#' coef(model)
#' @export
ridgereg <- function(formula, data, lambda) {
  
  # Create model matrix X (predictors) and response vector y
  X <- model.matrix(formula, data)
  y <- data[[all.vars(formula)[1]]]
  
  # Separate the intercept and predictors for normalization
  intercept <- X[, 1]  # Store intercept
  X_predictors <- X[, -1]  # Remove intercept for scaling
  
  # Scale the predictor columns (excluding intercept)
  X_norm <- scale(X_predictors)
  X_norm <- cbind(intercept, X_norm)  # Add the intercept column back
  
  # Add λ to the diagonal of X'X (no penalty on the intercept)
  lambda_I <- diag(lambda, ncol(X_norm))
  lambda_I[1, 1] <- 0  # Intercept column not penalized
  
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
print.ridgereg <- function(object) {
  cat("Call:\n")
  cat("ridgereg(formula = ", deparse(object$formula), ", data = ", deparse(substitute(object$data)), ", lambda = ", object$lambda, ")\n\n")
  
  # Print the coefficients
  cat("Coefficients:\n")
  coef_names <- colnames(model.matrix(object$formula, object$data))
  coefs <- as.vector(object$coefficients)
  names(coefs) <- coef_names
  print(coefs)
}

#' @export
predict.ridgereg <- function(object, newdata = NULL) {
  # If newdata is provided, normalize it based on the training data's scaling
  if (!is.null(newdata)) {
    X_new <- model.matrix(object$formula, newdata)
    X_new_norm <- scale(X_new, center = attr(scale(object$data, center = TRUE, scale = TRUE), "scaled:center"), 
                        scale = attr(scale(object$data, center = TRUE, scale = TRUE), "scaled:scale"))
  } else {
    # Use original data
    X_new_norm <- scale(model.matrix(object$formula, object$data), center = TRUE, scale = TRUE)
  }
  
  # Return the predicted values
  return(X_new_norm %*% object$coefficients)
}

#' @export
coef.ridgereg <- function(object) {
  # Return the coefficients as a named vector
  coef_names <- colnames(model.matrix(object$formula, object$data))
  coefs <- as.vector(object$coefficients)
  names(coefs) <- coef_names
  return(coefs)
}
