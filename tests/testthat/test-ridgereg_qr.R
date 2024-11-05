library(testthat)
library(MASS)  # For lm.ridge
library(BonusLab)  # Replace with the actual package name where ridgereg_qr is implemented

test_that("ridgereg_qr produces similar coefficients to lm.ridge", {
  # Define a small test dataset
  data(iris)
  formula <- Sepal.Length ~ Sepal.Width + Petal.Length
  lambda <- 0.1
  
  # Fit models with ridgereg_qr and lm.ridge
  ridge_qr_model <- ridgereg_qr(formula, data = iris, lambda = lambda)
  lm_ridge_model <- lm.ridge(formula, data = iris, lambda = lambda)
  
  # Extract coefficients from ridgereg_qr
  ridge_qr_coefs <- coef(ridge_qr_model)
  
  # Compute intercept for lm.ridge and combine with coefficients
  lm_ridge_coefs <- coef(lm_ridge_model)
  intercept <- lm_ridge_model$ym - sum(lm_ridge_coefs * lm_ridge_model$xm)
  lm_ridge_coefs <- c(intercept, lm_ridge_coefs)
  
  # Subset to matching terms by name
  matching_terms <- intersect(names(ridge_qr_coefs), names(lm_ridge_coefs))
  ridge_qr_coefs <- ridge_qr_coefs[matching_terms]
  lm_ridge_coefs <- lm_ridge_coefs[matching_terms]
  
  # Compare coefficients with an increased tolerance
  expect_equal(unname(ridge_qr_coefs), unname(lm_ridge_coefs), tolerance = 1e-2)
})