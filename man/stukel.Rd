\name{stukel}
\alias{stukel}
\title{Stukels test of the logistic link}
\usage{
  stukel(object,
    alternative = c("both", "alpha1", "alpha2"))
}
\arguments{
  \item{object}{An object of class \bold{glm}}

  \item{alternative}{add both \eqn{z1} and \eqn{z2} to
  model or just one of the above}
}
\value{
  A list with the following elemens: \item{statistic}{value
  of statistic} \item{parameter}{degrees of freedom}
  \item{p.value}{if \eqn{<0.05} suggests should accept null
  hypothesis that logistic model is incorrect for the data}
  \item{alternative}{alternative} \item{method}{method}
  \item{data.name}{name of object}
  \item{allstat}{statistics for all tests}
  \item{allpar}{degrees of freedom} \item{allpval}{all p
  values}
}
\description{
  Calculates Stukels test for a logistic regression model.
  \cr \cr This determines the appropriateness of the
  logistic link. \cr \cr Two new covariates, z1 and z2 are
  generated such that \deqn{ z1=0.5*logit^2*I(pi>=0.5),
  z2=-0.5*logit^2*I(pi<=0.5) } where \eqn{I(arg)=1} where
  arg is true and \eqn{=0} where false. \cr \cr Tests if
  significant change occurs in the model with the addition
  of these coefficients.
}
\examples{
set.seed(1)
m1 <- genLogiDf(b=3,f=0,c=0,n=50)$model
stukel(m1)
}
\author{
  Brett Presnell
}
\references{
  \href{http://http://www.stat.ufl.edu/~presnell/Courses/sta7249-2008sp/R/Src/stukel.R}{University
  of Florida}
}
\keyword{htest}

