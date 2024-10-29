\name{BonusLab-package}
\alias{BonusLab-package}
\alias{BonusLab}
\docType{package}
\title{Tools for Ridge Regression and Data Visualization}

\description{
The `BonusLab` package provides tools to perform ridge regression and visualizing data.
}

\details{
This package has functions like `ridgereg` for ridge regression and `visualize_airport_delays` to plot mean flight delays by airport.
}

\author{
Package Author: Ana Stanojcic
Maintainer: Ana <anastanojcic03@gmail.com>
}

\references{
For additional information see Chapter 3.4.1 in "An Introduction to Statistical Learning" by James, Witten, Hastie, and Tibshirani.
}

\seealso{
\code{\link[MASS:lm.ridge]{lm.ridge}} in the MASS package for a comparable ridge regression function.
}

\examples{
# Example of ridge regression with `ridgereg`
data(iris)
model <- ridgereg(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris, lambda = 0.1)
print(model)

# Example of visualizing airport delays
visualize_airport_delays()
}

\keyword{package}
