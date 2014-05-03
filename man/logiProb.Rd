\name{logiProb}
\alias{logiProb}
\title{Logit, odds ratio and probability for coefficients of a
logistic regression}
\usage{
  logiProb(x, usePrim = FALSE, all = FALSE)
}
\arguments{
  \item{x}{A logistic regression model of class \code{glm}}

  \item{all}{If \code{all=FALSE} (the default) return
  values for all coefficients in model, considered
  together. \cr If \code{all=TRUE} return values for all
  \emph{combinations} of coefficients in model.}

  \item{usePrim}{If \code{usePrim=FALSE} (the default) use
  \code{utils::combn} to generate combinations. \cr If
  \code{usePrim=TRUE} use \code{gRbase::combnPrim} instead
  (faster).}
}
\value{
  If \code{all=TRUE}, a \code{data.table} giving, for each
  \emph{combination} of coefficients:
  \item{coef}{Combination of coefficients} \item{logit}{The
  logit for a given combination of coefficients}
  \item{lnOR}{Natural log of Odds Ratio} \item{OR}{Odds
  Ratio} \item{p}{probability} This is sorted by \code{OR}
  (low to high). \cr \cr If \code{all=FALSE}, a
  \code{data.frame} giving the above values for all
  predictors.
}
\description{
  Generate logit, log odds ratio, odds ratio and
  probability for coefficients in a logistic regression.
  Can be generalized to all combinations of coefficients.
  \cr \cr Values are calculated for a change in the value
  of the coeffient for the predictor from \eqn{0} to
  \eqn{1}. (For continuous predictors changes of more than
  one unit may have more practical significance).
}
\note{
  To use \code{gRbase::combnPrim} the following
  dependencies may be necessary. Install as follows: \cr
  source("http://bioconductor.org/biocLite.R") \cr
  biocLite("graph") \cr biocLite("BiocGenerics") \cr
  biocLite("RBGL") \cr
}
\examples{
set.seed(1)
f1 <- genLogiDf(n=50)$model
logiProb(f1)
d1 <- genLogiDt(n=50, model=FALSE)
logiProb(glm(y ~ x1 + x3 -1, data=d1, family=binomial()), all=TRUE)
}

