\name{logiGOF}
\alias{logiGOF}
\title{Goodness of fit tests for a logistic regression model}
\usage{
  logiGOF(x, g = 10)
}
\arguments{
  \item{x}{A model of class \code{glm}}

  \item{g}{No. groups (quantiles) into which to split
  observations for Hosmer-Lemeshow and modified
  Hosmer-Lemeshow tests.}
}
\value{
  A \code{list} of class \code{logiGOF} with the following
  items: \item{chiPearCov}{Pearsons chi-square, calculated
  by \emph{covariate group}, with \eqn{p} value and
  interpretation} \item{chiPearIndiv}{Pearsons chi-square,
  calculated by \emph{individual observation}, with \eqn{p}
  value and interpretation} \item{chiPearTab}{Pearsons
  chi-square, calculated by \emph{table of covariate
  patterns by outcome}, with \eqn{p} value and
  interpretation} \item{OsRo}{Osius & Rojek test of the
  logistic link, with \eqn{p} value and interpretation}
  \item{chiDevCov}{Deviance chi-square, calculated by
  \emph{covariate group}, with \eqn{p} value and
  interpretation} \item{chiDevIndiv}{Deviance chi-square,
  calculated by \emph{individual observation}, with \eqn{p}
  value and interpretation} \item{chiDevTab}{Deviance
  chi-square, calculated by \emph{table of covariate
  patterns by outcome}, with \eqn{p} value and
  interpretation} \item{covPatTab}{Matrix of covariance
  patterns, used to calculate above chi-square tests of
  Pearson residuals and deviance} \item{HosLem}{Hosmer &
  Lemeshow goodness of fit test, with \code{g} quantile
  groups,with \eqn{p} value and interpretation}
  \item{modHosLem}{modified Hosmer & Lemeshow goodness of
  fit test, with \code{g} quantile groups, with \eqn{p}
  value and interpretation} \item{CesHou}{le Cessie, van
  Houwelingen, Copas & Hosmer unweighted sum of squares
  test for global goodness of fit, with \eqn{p} value and
  interpretation} \item{Stuk}{Stukels test of the
  appropriateness of the logistic link, with \eqn{p} value
  and interpretation} \item{PR2}{Pearsons R^2, correlation
  of observed outcome with predicted} \item{ssR2}{Linear
  regression-like sum-of-squares R^2, using covariate
  patterns} \item{llR2}{Log-likelohood based R^2,
  calculated by covariate group} \item{ROC}{Area under the
  Receiver Operating Curve, with 95\% CI by method of
  DeLong}
}
\description{
  Gives 15 commonly employed measures of goodness of fit
  for a logistic regression model
}
\note{
  A \code{summary} method is available \cr \cr Warning:
  Will fail if cannot generate a hat matrix for the model
  using \code{logiDx}
}
\examples{
set.seed(1)
m1 <- genLogiDf(n=100)$model
logiGOF(m1)
}
\author{
  Modified Hosmer & Lemeshow goodness of fit test: adapted
  from existing work by Yongmei Ni
}
\seealso{
  \code{\link{logiDx}}
}
\keyword{htest}

