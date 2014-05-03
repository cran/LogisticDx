\name{logiSS}
\alias{logiSS}
\title{Sample size for given coefficient and events per covariate for model}
\usage{
  logiSS(x, alpha = 0.05, beta = 0.8, coeff = "x1")
}
\arguments{
  \item{x}{A logistic regression model of class \code{glm}}

  \item{alpha}{significance level \eqn{\alpha} for
  null-hypothesis significance test}

  \item{beta}{power \eqn{\beta} for null-hypothesis
  significance test}

  \item{coeff}{Name of predictor (coefficient) in model to
  be tested}
}
\value{
  A list of: \item{res}{Result: Sample size required to
  show coefficient for predictor is as given in the model
  rather than 0} \item{epc}{Events per covariate; should be
  >10 to make meaningful statements about coefficients
  obtained}
}
\description{
  Gives sample size necessary to demonstrate that
  coefficient in model for given predictor is equal to its
  given value (rather than equal to zero) for a given level
  of power and significance. \cr \cr Also number of events
  (smaller of outcome \eqn{y=0} and outcome \eqn{y=1}) per
  predictor. \cr \cr Uses different methods depending on
  whether model has one binomial, one continuous or
  multiple predictors.
}
\examples{
set.seed(1)
### one coefficient, which is binomial
f1 <- genLogiDf(b=1, c=0, n=50)$model
logiSS(f1)
###
### one coefficient, which is continuous
f1 <- genLogiDf(f=0, b=0, c=1, n=50)$model
logiSS(f1, coeff="x1")
###
### binomial coefficient
f1 <- genLogiDf(f=0, b=1, c=1, n=50)$model
logiSS(f1, coeff="x1")
###
### continuous coefficient
f1 <- genLogiDf(f=0, b=1, c=1, n=50)$model
logiSS(f1, coeff="x2")
}
\keyword{htest}

