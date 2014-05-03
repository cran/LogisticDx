\name{genLogi}
\alias{genLogi}
\alias{genLogiDf}
\alias{genLogiDt}
\title{Generate data for logistic regression}
\usage{
  genLogiDf(b = 2L, f = 2L, c = 1L, n = 20L, nlf = 3L,
    pb = 0.5, rc = 0.8, py = 0.5, asFactor = TRUE,
    model = TRUE, timelim = 5, speedglm = FALSE)

  genLogiDt(b = 2L, f = 2L, c = 1L, n = 20L, nlf = 3L,
    pb = 0.5, rc = 0.8, py = 0.5, asFactor = TRUE,
    model = TRUE, timelim = 5, speedglm = FALSE)
}
\arguments{
  \item{b}{\dfn{binomial predictors}, the number of
  predictors which are binary, i.e. limited to \eqn{0} or
  \eqn{1}}

  \item{f}{\dfn{factors}, the number of predictors which
  are factors}

  \item{c}{\dfn{continuous predictors}, the number of
  predictors which are continuous}

  \item{n}{number of observations in the data frame}

  \item{nlf}{the no. of levels in a factor}

  \item{pb}{\dfn{probability for binomnial predictors}: the
  probability of binomial predictors being \eqn{=1} e.g. if
  \code{pb=0.3}, \eqn{30\%} will be \eqn{1}s, \eqn{70\%}
  will be \eqn{0}s}

  \item{rc}{\dfn{ratio for continuous variables} the ratio
  of levels of continuous variables to the total number of
  observations \dfn{n} e.g. if \code{rc=0.8} and
  \code{n=100}, it will be in the range 1-80}

  \item{py}{\dfn{ratio for y} the ratio of 1s to total
  observations for the binomial predictors e.g. if
  \code{ry=0.5}, 50\% will be \eqn{1}s, \eqn{50\%} will be
  \eqn{0}s}

  \item{asFactor}{If \code{asFactor=TRUE} (the default),
  predictors given as \code{factor}s will be converted to
  \code{factor}s in the data frame before the model is fit}

  \item{model}{If \code{model=TRUE} will also return a
  model fitted with \code{stats::glm} or
  \code{speedglm::speedglm}}

  \item{timelim}{function will timeout after \code{timelim}
  secs. This is present to prevent duplication of rows.}

  \item{speedglm}{If \code{speedglm=TRUE}, return a model
  fitted with \code{speedglm} instead of \code{glm}}
}
\value{
  If \code{model=TRUE}: a list with the following values:
  \item{df or dt}{A \code{data.frame} (for
  \code{genLogiDf}) or \code{data.table} (for
  \code{genLogiDt}). \cr Predictors are labelled \eqn{x1,
  x2, ..., xn}. \cr Outcome is \eqn{y}. \cr Rows represent
  to \eqn{n} observations} \item{model}{A model fit with
  \code{stats::glm} or \code{speedglm::speedglm}} If
  \code{model=FALSE} a \code{data.frame} or
  \code{data.table} as above.
}
\description{
  Generates a \code{data.frame} or \code{data.table} with a
  binary outcome, and a logistic model to describe it.
}
\note{
  \code{genLogiDt} is faster and more efficient for larger
  datasets. \cr \cr Using \code{asFactor=TRUE} with factors
  which have a large number of levels (e.g. \code{nlf >30})
  on large datasets (e.g. \eqn{n >1000}) can cause fitting
  to be excessively slow.
}
\examples{
set.seed(1)
genLogiDf()
genLogiDt(b=0, c=2, n=100, rc=0.7, model=FALSE)
}
\keyword{datagen}

