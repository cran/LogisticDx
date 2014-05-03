\name{logiDx}
\alias{logiDx}
\title{Diagnostics for logistic regression}
\usage{
  logiDx(x, round = FALSE, roundTo = 3)
}
\arguments{
  \item{x}{A model of class \code{glm}}

  \item{round}{If \code{round=TRUE}, digits will be ronded
  to \code{roundTo} decimal places}

  \item{roundTo}{No. decimal places to which to round
  digits}
}
\value{
  A \code{data.table}. There is one row per covariate
  pattern with at least one observation. These are sorted
  by \code{dBhat} (see below). \cr \cr The initial columns
  give all combinations of the predictor variables with at
  least one observation. \cr \cr Subsequent columns are
  labelled as follows: \item{obs}{Number of observations
  with this covariate pattern} \item{prob}{Probability of
  this covariate pattern} \item{yhat}{Number of
  observations of \eqn{y=1}, \emph{predicted} by the model}
  \item{y}{\emph{Actual} number of observations of
  \eqn{y=1} from the data} \item{lev}{\dfn{Leverage}, the
  diagonal of the hat matrix used to generate the model; a
  measure of influence of this covariate pattern}
  \item{devR}{\dfn{Deviance residual}, calculated by
  covariate pattern; a measure of influence of this
  covariate pattern} \item{PeR}{\dfn{Pearson residual},
  calculated by covariate pattern; a measure of influence
  of this covariate pattern. Given by: \deqn{
  \sqrt{obs}\sqrt{\frac{prob}{(1-prob)}}}{ obs^0.5
  (prob/1-prob)^0.5}} \item{sPeR}{\dfn{Standardized Pearson
  residual} calculated by covariate pattern; a measure of
  influence of this covariate pattern. Given by: \deqn{
  \frac{PeR}{\sqrt{(1-lev)}}}{ PeR.(1-lev)^0.5}}
  \item{dBhat}{\dfn{Change in Bhat}, the standardized
  difference between the original maximum likelihood
  estimates \bold{B} and that estimates with this covariate
  pattern excluded} \item{dXsq}{\dfn{Change in chi-square},
  decrease in the value of Pearson chi-square statistic
  with this covariate pattern excluded. Given by:
  \deqn{sPeR^2}} \item{dDev}{\dfn{Change in deviance}
  \bold{D} with this covariate pattern excluded. Given by:
  \deqn{ \frac{dev^2}{(1-lev)}}{ d^2/(1-lev)}}
}
\description{
  Returns standard diagnostic measures for a logistic
  regression model by covariate pattern
}
\note{
  Values for the statistics are calculated by
  \emph{covariate pattern}. Different values may be
  obtained if calculated for each individual obervation
  (i.e. row in data frame). \cr \cr Generally, the values
  calculated by covariate pattern are preferred,
  particularly where no. observations are \eqn{>5}.
}
\examples{
d1 <- genLogiDt(model=FALSE)
f1 <- stats::glm(y ~ I(x5^2)*x1 -1, family=binomial("logit"), data=d1)
logiDx(f1)
}
\seealso{
  \code{\link{plotLogiDx}}
}

