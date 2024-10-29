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
  X <- model.matrix(predictor_formula, data)[, -1]  # Exclude intercept
  
  # Scale only predictor columns
  X_scaled <- scale(X)
  scaling_params <- list(
    center = attr(X_scaled, "scaled:center"),
    scale = attr(X_scaled, "scaled:scale")
  )
  
  # Add intercept column manually after scaling
  X_norm <- cbind(1, X_scaled)
  
  # Regularization matrix (no penalty on intercept)
  lambda_I <- diag(lambda, ncol(X_norm))
  lambda_I[1, 1] <- 0
  
  # Calculate ridge regression coefficients
  beta_ridge <- solve(t(X_norm) %*% X_norm + lambda_I) %*% t(X_norm) %*% y
  
  # Fitted values and residuals
  fitted_values <- X_norm %*% beta_ridge
  residuals <- y - fitted_values
  
  # Print diagnostics for scaling parameters
  cat("Scaling Parameters - Center:\n")
  print(scaling_params$center)
  cat("Scaling Parameters - Scale:\n")
  print(scaling_params$scale)
  
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
  cat("Using updated predict.ridgereg function\n")
  
  # Generate model matrix for new data without intercept
  predictor_formula <- reformulate(attr(terms(object$formula), "term.labels"))
  X_new <- model.matrix(predictor_formula, newdata)[, -1]  # Exclude intercept
  
  # Retrieve scaling parameters
  center <- object$scaling_params$center
  scale <- object$scaling_params$scale
  
  # Diagnostic print for scaling parameters
  cat("Predict - Center:\n")
  print(center)
  cat("Predict - Scale:\n")
  print(scale)
  
  # Apply scaling to predictor columns only
  X_new_scaled <- scale(X_new, center = center, scale = scale)
  
  # Add intercept column manually
  X_new_scaled <- cbind(1, X_new_scaled)
  
  # Check dimensions for debugging
  print(paste("Dimensions of X_new_scaled:", dim(X_new_scaled)))
  print(paste("Length of object$coefficients:", length(object$coefficients)))
  
  # Calculate predictions
  predicted <- X_new_scaled %*% object$coefficients
  
  # Warning for NaN values in predictions, if any
  if (any(is.nan(predicted))) {
    cat("Warning: NaN values detected in predictions\n")
  }
  
  return(predicted)
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

