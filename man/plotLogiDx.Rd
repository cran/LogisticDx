\name{plotLogiDx}
\alias{plotLogiDx}
\title{Diagnostic plots for a logistic regression}
\usage{
  plotLogiDx(x, noPerPage = 6,
    cols = c("deepskyblue", "dodgerblue"), cex = 2,
    pch = 21, inches = 0.25, identify = FALSE,
    extras = FALSE, width = NULL, height = NULL)
}
\arguments{
  \item{x}{A logistic regression model of class \code{glm}}

  \item{noPerPage}{Number of plots per page (for initial
  plots). Will be used as \emph{guidance} and optimised for
  ease of display}

  \item{cols}{Colours. As used by \code{graphics::points}}

  \item{cex}{Cex \bold{C}haracter \bold{ex}pansion. See \cr
  \code{?graphics::plot.default}}

  \item{pch}{\bold{P}lotting \bold{ch}aracter. See \cr
  \code{?graphics::points}}

  \item{inches}{Width of circles for bubble plot. See \cr
  \code{?graphics::symbols}}

  \item{identify}{If \code{TRUE} will give option to
  identify individual points on a number of the plots
  produced. \cr The number which appears next to the point
  corresponds to the relevant row as given by
  \code{\link{logiGOF}}}

  \item{extras}{If \code{TRUE} produces additional plots,
  detailed below}

  \item{width}{Width of screen(display device) in pixels}

  \item{height}{Height of screen(display device) in pixels}
}
\value{
  The following are plotted, for each covariate group:

  \item{p_X_lev}{Probability of \eqn{y=1} for this group by
  leverage (diagonal of hat matrix, a measure of
  influence)} \item{p_X_dXsq}{Probability as above by
  \dfn{dXsq} change in Pearson chi-square statistic with
  deletion of this group} \item{p_X_dBhat}{Probability by
  \dfn{dBhat} change in Bhat; the difference in the maximum
  likelihood estimators \bold{Beta} for model coefficients
  with all subjects included vs those with this group,
  standardized by the estimated covariance matrix of
  \bold{Beta}} \item{p_X_dDev}{Probability by \dfn{dDev},
  the change in deviance when this group is excluded}
  \item{bubbleplot}{Probability by dXsq, with area of
  circle proportional to dBhat} \item{lev_X_dXsq}{Leverage
  by \dfn{dXsq}, the change in the Pearson chi-square
  statistic when this group is excluded}
  \item{lev_X_dBhat}{Leverage by \dfn{dBhat}, the
  difference in the maximum likelihood estimators
  \bold{Beta} for model coefficients with all subjects
  included vs those when this group is excluded. This is
  standardized by the estimated covariance matrix of
  \bold{Beta}} \item{lev_X_dDev}{Leverage by \dfn{dDev},
  the change in deviance when this group is excluded}
  \item{ROC}{Receiver Operator Curve}

  Additional plots are given when \code{extras=TRUE}:

  \item{influenceplot}{See \cr \code{?car::influencePlot}}
  \item{sr_X_hat}{\dfn{Studentized residual} by hat values.
  Studentized residual = residual / estimate of standard
  deviation of residual} \item{slp}{Spread-level plot. See
  \cr from \code{?car::spreadLevelPlot}}
  \item{qqPlot}{quantile-quantile plot vs Normal for
  residuals. See \cr \code{?stats::qqplot}}
  \item{iip}{Influence-index plot. Gives Cooks distance,
  studentized residual and hat values for each observation}
  \item{pairs}{Pairs plot for the measures of influence
  dBhat, dXsq and dDev. See \cr \code{?graphics::pairs}}
  \item{crPlots}{Component + residual plots. See \cr
  \code{?car::crPlots}} \item{avPlots}{Added-variable
  plots. See \cr \code{?car::avPlots}} \item{mmps}{Marginal
  model plots. These require that the \code{data.frame}
  used to fit the model be present in the current
  environment. See \cr \code{?car::mmps}}
}
\description{
  Common diagnostic plots for a logistic regression model
}
\note{
  Different colors can be found with e.g. \cr
  \code{grDevices::colours()[grep("blue",grDevices::colours())]}
}
\examples{
set.seed(1)
### generate up to 8x covariate patterns
mod1 <- genLogiDf(b=3, f=0, c=0, n=50)$model
plotLogiDx(mod1, cex=8, noPerPage=1)
plotLogiDx(mod1, cex=3, noPerPage=6, extras=TRUE)
df1 <- genLogiDf(b=0,f=0,c=2,n=50, model=FALSE)
g1 <- glm(y ~ ., family=binomial("logit"), data=df1)
plotLogiDx(g1)
}
\keyword{hplot}

