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
  
  # Extract coefficients from both models, ensuring intercepts align
  ridge_coefs <- coef(ridge_model)
  lm_ridge_coefs <- c(lm_ridge_model$ym - sum(lm_ridge_coefs * lm_ridge_model$xm), lm_ridge_coefs)
  
  # Compare coefficients with a tolerance for numerical differences
  expect_equal(ridge_coefs, lm_ridge_coefs, tolerance = 1e-6)
})

