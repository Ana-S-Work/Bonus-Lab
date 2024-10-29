#' @title Ridge Regression with Least Squares
#' @description Perform ridge regression using least squares, following the formula instructions.
#' @param formula A formula object, e.g., y ~ x1 + x2.
#' @param data A data frame containing the variables in the model.
#' @param lambda A numeric value specifying the ridge penalty parameter λ.
#' @return An object of class `ridgereg` containing ridge regression results.
#' @examples
#' model <- ridgereg(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris, lambda = 0.1)
#' print(model)
#' predict(model, newdata = iris)
#' coef(model)
#' @export
ridgereg <- function(formula, data, lambda) {
  # Extract response and predictor variables
  y <- data[[all.vars(formula)[1]]]
  X <- model.matrix(formula, data)  # Automatically includes intercept
  
  # Normalize predictor columns (excluding intercept)
  X_centered <- scale(X[, -1], center = TRUE, scale = TRUE)  # Exclude intercept column
  X_norm <- cbind(1, X_centered)  # Add intercept column back
  
  # Calculate ridge regression coefficients using least squares
  lambda_I <- diag(lambda, ncol(X_norm))
  lambda_I[1, 1] <- 0  # Do not penalize the intercept
  
  # Calculate ridge coefficients
  beta_ridge <- solve(t(X_norm) %*% X_norm + lambda_I) %*% t(X_norm) %*% y
  
  # Calculate fitted values and residuals
  fitted_values <- X_norm %*% beta_ridge
  residuals <- y - fitted_values
  
  # Return model object
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
  cat("Coefficients:\n")
  print(as.vector(object$coefficients))
}

#' @export
predict.ridgereg <- function(object, newdata = NULL) {
  X_new <- model.matrix(object$formula, newdata)
  predictions <- X_new %*% object$coefficients
  return(predictions)
}

#' @export
coef.ridgereg <- function(object) {
  return(as.vector(object$coefficients))
}
