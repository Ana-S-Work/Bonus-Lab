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
  # Extract response and predictor variables
  y <- data[[all.vars(formula)[1]]]
  predictor_formula <- reformulate(attr(terms(formula), "term.labels"))
  X <- model.matrix(predictor_formula, data)
  
  # Separate intercept and predictors for normalization
  intercept <- X[, 1]
  X_predictors <- X[, -1]
  
  # Scale predictors, excluding intercept
  X_norm <- scale(X_predictors)
  X_norm <- cbind(1, X_norm)  # Add intercept column back
  
  # Store scaling parameters to use later in predict method
  scaling_params <- list(
    center = attr(X_norm, "scaled:center"),
    scale = attr(X_norm, "scaled:scale")
  )
  
  # Regularization matrix, excluding intercept
  lambda_I <- diag(lambda, ncol(X_norm))
  lambda_I[1, 1] <- 0
  
  # Ridge regression coefficients
  beta_ridge <- solve(t(X_norm) %*% X_norm + lambda_I) %*% t(X_norm) %*% y
  
  # Fitted values and residuals
  fitted_values <- X_norm %*% beta_ridge
  residuals <- y - fitted_values
  
  # Return model object
  model <- list(
    formula = formula,
    data = data,
    lambda = lambda,
    coefficients = beta_ridge,
    fitted_values = fitted_values,
    residuals = residuals,
    scaling_params = scaling_params
  )
  class(model) <- "ridgereg"
  return(model)
}


#' @export
print.ridgereg <- function(object) {
  cat("Call:\n")
  cat("ridgereg(formula = ", deparse(object$formula), ", data = ", deparse(substitute(object$data)), ", lambda = ", object$lambda, ")\n\n")
  
  # Print the coefficients
  coef_names <- colnames(model.matrix(reformulate(attr(terms(object$formula), "term.labels")), object$data))
  coefs <- as.vector(object$coefficients)
  
  # Adjust names to include "(Intercept)" if there is an intercept
  if (length(coefs) == length(coef_names) + 1) {
    names(coefs) <- c("(Intercept)", coef_names)
  } else {
    names(coefs) <- coef_names
  }
  
  print(coefs)
}
#' @export
predict.ridgereg <- function(object, newdata = NULL) {
  predictor_formula <- reformulate(attr(terms(object$formula), "term.labels"))
  
  # Generate model matrix for new data
  if (!is.null(newdata)) {
    X_new <- model.matrix(predictor_formula, newdata)
  } else {
    X_new <- model.matrix(predictor_formula, object$data)
  }
  
  # Retrieve saved scaling parameters
  center <- object$scaling_params$center
  scale <- object$scaling_params$scale
  
  # Apply scaling to new data based on training data parameters
  X_new_norm <- scale(X_new, center = center, scale = scale)
  
  # Add intercept column to ensure dimensions match
  X_new_norm <- cbind(1, X_new_norm)
  
  # Print dimensions for debugging
  print(paste("Dimensions of X_new_norm:", dim(X_new_norm)))
  print(paste("Length of object$coefficients:", length(object$coefficients)))
  
  # Return the predicted values
  return(X_new_norm %*% object$coefficients)
}


#' @export
coef.ridgereg <- function(object) {
  # Return the coefficients as a named vector
  coef_names <- colnames(model.matrix(reformulate(attr(terms(object$formula), "term.labels")), object$data))
  coefs <- as.vector(object$coefficients)
  
  # Adjust names to include "(Intercept)" if there is an intercept
  if (length(coefs) == length(coef_names) + 1) {
    names(coefs) <- c("(Intercept)", coef_names)
  } else {
    names(coefs) <- coef_names
  }
  
  return(coefs)
}

