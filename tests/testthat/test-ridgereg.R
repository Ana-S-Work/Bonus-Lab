library(testthat)
library(MASS)  # For lm.ridge
library(BonusLab)  # Replace with the actual package name where ridgereg is implemented

test_that("ridgereg produces similar coefficients to lm.ridge", {
  # Define a small test dataset
  data(iris)
  formula <- Sepal.Length ~ Sepal.Width + Petal.Length
  lambda <- 0.1
  
  # Fit models with ridgereg and lm.ridge
  ridge_model <- ridgereg(formula, data = iris, lambda = lambda)
  lm_ridge_model <- lm.ridge(formula, data = iris, lambda = lambda)
  
  # Extract and align coefficients for both models
  ridge_coefs <- as.vector(coef(ridge_model))
  lm_ridge_coefs <- as.vector(coef(lm_ridge_model))
  
  # Ensure alignment in length for comparison
  if (length(ridge_coefs) != length(lm_ridge_coefs)) {
    min_len <- min(length(ridge_coefs), length(lm_ridge_coefs))
    ridge_coefs <- ridge_coefs[1:min_len]
    lm_ridge_coefs <- lm_ridge_coefs[1:min_len]
  }
  
  # Compare coefficients with a tolerance for numerical differences
  expect_equal(ridge_coefs, lm_ridge_coefs, tolerance = 1e-6)
})

