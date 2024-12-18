% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ridgereg.R
\name{ridgereg}
\alias{ridgereg}
\title{Ridge Regression with Least Squares}
\usage{
ridgereg(formula, data, lambda)
}
\arguments{
\item{formula}{A formula object, e.g., y ~ x1 + x2.}

\item{data}{A data frame containing the variables in the model.}

\item{lambda}{A numeric value specifying the ridge penalty parameter λ.}
}
\value{
An object of class `ridgereg` containing ridge regression results.
}
\description{
Perform ridge regression using least squares, following the formula instructions.
}
\examples{
model <- ridgereg(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris, lambda = 0.1)
print(model)
predict(model, newdata = iris)
coef(model)
}
