# Ridge regression function using QR decomposition
ridgereg_qr <- function(formula, data, lambda) {
  # Extract response and predictor variables
  y <- data[[all.vars(formula)[1]]]
  X <- model.matrix(formula, data)
  
  # Perform QR decomposition on the predictor matrix
  qr_decomp <- qr(X)
  Q <- qr.Q(qr_decomp)
  R <- qr.R(qr_decomp)
  
  # Adjust R matrix to include the ridge penalty
  n <- ncol(R)
  R_ridge <- rbind(R, sqrt(lambda) * diag(n))
  
  # Adjust response vector y
  y_ridge <- c(crossprod(Q, y), rep(0, n))
  
  # Solve the system for ridge coefficients
  beta_ridge <- solve(t(R_ridge) %*% R_ridge) %*% t(R_ridge) %*% y_ridge
  
  # Calculate fitted values and residuals
  fitted_values <- X %*% beta_ridge
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
  class(model) <- "ridgereg_qr"
  return(model)
}

#' @export
print.ridgereg_qr <- function(x, ...) {
  cat("Call:\n")
  cat("ridgereg_qr(formula = ", deparse(x$formula), ", data = ", deparse(substitute(x$data)), ", lambda = ", x$lambda, ")\n\n")
  cat("Coefficients:\n")
  print(as.vector(x$coefficients))
}

#' @export
predict.ridgereg_qr <- function(object, newdata = NULL) {
  X_new <- model.matrix(object$formula, newdata)
  predictions <- X_new %*% object$coefficients
  return(predictions)
}

#' @export
coef.ridgereg_qr <- function(object) {
  return(as.vector(object$coefficients))
}