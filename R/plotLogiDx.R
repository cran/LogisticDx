##' @name plotLogiDx
##' @export
##' @include logiDx.R
##' @include multiPlot.R
##' @include genLogi.R
##' @title Diagnostic plots for a logistic regression
##'
##' @description
##' Common diagnostic plots for a logistic regression model
##'
##' @param x A logistic regression model of class \code{glm}
##' @param noPerPage Number of plots per page (for initial plots).
##' Will be used as \emph{guidance} and optimised for ease of display
##' @param cols Colours. As used by \code{graphics::points}
##' @param cex Cex \bold{C}haracter \bold{ex}pansion. See
##' \cr
##' \code{?graphics::plot.default}
##' @param pch \bold{P}lotting \bold{ch}aracter. See
##' \cr
##' \code{?graphics::points}
##' @param inches Width of circles for bubble plot. See
##' \cr
##' \code{?graphics::symbols}
##' @param identify If \code{TRUE} will give option to identify
##' individual points on a number of the plots produced.
##' \cr
##' The number which appears next to the point corresponds to the relevant row
##' as given by \code{\link{logiGOF}}
##' @param extras If \code{TRUE} produces additional plots, detailed below
##' @param width Width of screen(display device) in pixels
##' @param height Height of screen(display device) in pixels
##' @return The following are plotted, for each covariate group:
##'
##' \item{p_X_lev}{Probability of \eqn{y=1} for this group
##' by leverage (diagonal of hat matrix, a measure of influence)}
##' \item{p_X_dXsq}{Probability as above by \dfn{dXsq} change in Pearson chi-square
##' statistic with deletion of this group}
##' \item{p_X_dBhat}{Probability by \dfn{dBhat} change in Bhat; the difference in
##' the maximum likelihood estimators \bold{Beta} for model coefficients with
##' all subjects included vs those with this group, standardized by the
##' estimated covariance matrix of \bold{Beta}}
##' \item{p_X_dDev}{Probability by \dfn{dDev}, the change in deviance when this group
##' is excluded}
##' \item{bubbleplot}{Probability by dXsq, with area of circle proportional to dBhat}
##' \item{lev_X_dXsq}{Leverage by \dfn{dXsq}, the change in the Pearson chi-square statistic
##' when this group is excluded}
##' \item{lev_X_dBhat}{Leverage by \dfn{dBhat}, the difference in
##' the maximum likelihood estimators \bold{Beta} for model coefficients with
##' all subjects included vs those when this group is excluded. This is standardized by the
##' estimated covariance matrix of \bold{Beta}}
##' \item{lev_X_dDev}{Leverage by \dfn{dDev}, the change in deviance when this group
##' is excluded}
##' \item{ROC}{Receiver Operator Curve}
##'
##' Additional plots are given when \code{extras=TRUE}:
##'
##' \item{influenceplot}{See
##' \cr
##' \code{?car::influencePlot}}
##' \item{sr_X_hat}{\dfn{Studentized residual} by hat values.
##' Studentized residual = residual / estimate of standard deviation of residual}
##' \item{slp}{Spread-level plot. See
##' \cr
##' from \code{?car::spreadLevelPlot}}
##' \item{qqPlot}{quantile-quantile plot vs Normal for residuals. See
##' \cr
##' \code{?stats::qqplot}}
##' \item{iip}{Influence-index plot. Gives Cooks distance, studentized residual and hat
##' values for each observation}
##' \item{pairs}{Pairs plot for the measures of influence dBhat, dXsq and dDev. See
##' \cr
##' \code{?graphics::pairs}}
##' \item{crPlots}{Component + residual plots. See
##' \cr
##' \code{?car::crPlots}}
##' \item{avPlots}{Added-variable plots. See
##' \cr
##' \code{?car::avPlots}}
##' \item{mmps}{Marginal model plots. These require that the \code{data.frame} used
##' to fit the model be present in the current environment. See
##' \cr
##' \code{?car::mmps}}
##'
##' @note Different colors can be found with e.g.
##' \cr
##' \code{grDevices::colours()[grep("blue",grDevices::colours())]}
##'
##' @keywords hplot
##' @examples
##'
##' set.seed(1)
##' ### generate up to 8x covariate patterns
##' mod1 <- genLogiDf(b=3, f=0, c=0, n=50)$model
##' plotLogiDx(mod1, cex=8, noPerPage=1)
##' plotLogiDx(mod1, cex=3, noPerPage=6, extras=TRUE)
##' df1 <- genLogiDf(b=0,f=0,c=2,n=50, model=FALSE)
##' g1 <- glm(y ~ ., family=binomial("logit"), data=df1)
##' plotLogiDx(g1)
##'
plotLogiDx <- function(x, noPerPage=6,
                       cols=c("deepskyblue", "dodgerblue"),
                       cex=2, pch=21,
                       inches=0.25,
                       identify=FALSE,
                       extras=FALSE,
                       width=NULL, height=NULL) {
    stopifnot(inherits(x, "glm"))
    prob <- lev <- dXsq <- dBhat <- dDev <- NULL
### get diagnostics for model
    dx1 <- logiDx(x)
### set up plot area and store default parameters
    p <- .multiPlot(noPerPage=noPerPage, width=width, height=height)
###
    maintext <- function(){
        tex1 <- paste0("Diagnostic plots for logistic regression \n ",
                       deparse(x$formula))
        graphics::mtext(tex1, line=0.3, outer=TRUE)
        }
###-------------------------------------------
### probabiility by leverage
    graphics::plot(dx1[ ,prob], dx1[ ,lev],
                   xlab="Probability for this covariate pattern",
                   main="Probability by leverage",
                   ylab="Leverage (hat matrix diagonal)")
### to make horizontal on y-axis:
### graphics::mtext("Leverage \n (hat \n matrix \n diagonal)", side=2, line=3, las=1)
    graphics::points( dx1[ ,prob], dx1[ ,lev], pch=pch, cex=cex,
                     col=cols, bg=cols)
    if (identify) graphics::identify(dx1[ ,prob], dx1[ ,lev])
    maintext()
###-------------------------------------------
### probability by ...
### dXsq
    graphics::plot(dx1[ ,prob], dx1[ ,dXsq],
                   xlab="Probability for this covariate pattern",
                   ylab="dXsq = decrease in Pearson Chi-sq \n without this pattern",
                   main="dXsq by probability")
    graphics::points(dx1[ ,prob], dx1[ ,dXsq], pch=pch, cex=cex,
           col=cols, bg=cols)
    if (identify) graphics::identify(dx1[ ,prob], dx1[ ,dXsq])
    maintext()
### dBhat
    graphics::plot(dx1[ ,prob], dx1[ ,dBhat],
                   xlab="Probability for this covariate pattern",
                   ylab="dBhat = decrease in Bhat \n without this pattern",
                   main="dBhat by probability")
    graphics::points(dx1[ ,prob], dx1[ ,dBhat], pch=pch, cex=cex,
           col=cols, bg=cols)
    if (identify) graphics::identify(dx1[ ,prob], dx1[ ,dBhat])
    maintext()
### dDev
    graphics::plot(dx1[ ,prob], dx1[ ,dDev],
                   xlab="Probability for this covariate pattern",
                   ylab="dDev = decrease in Deviance \n without this pattern",
                   main="dDev by probability")
    graphics::points(dx1[ ,prob], dx1[ ,dDev], pch=pch, cex=cex,
           col=cols, bg=cols)
    if (identify) graphics::identify(dx1[ ,prob], dx1[ ,dDev])
    maintext()
###-------------------------------------------
### bubble plot - prob by dXsq, with area = dBhat
    radius <- sqrt(dx1[ ,dBhat]/ dx1[ ,prob])
    graphics::symbols(dx1[ ,prob], dx1[ ,dXsq],
                      circles=radius, inches=inches,
                      fg="white", bg=cols,
                      xlab="Probability for this covariate pattern",
                      ylab="dXsq = decrease in Pearson Chi-sq \n without this pattern",
                      main = "Area proportional to dBhat \n Decrease in Bhat without this pattern")
    if (identify) graphics::identify(dx1[ ,prob], dx1[ ,dXsq])
    maintext()

###---------------------------
### leverage by...
### dXsq
    graphics::plot(dx1[ ,lev], dx1[ ,dXsq],
                   xlab="Leverage for this covariate pattern",
                   ylab="dXsq = decrease in Pearson Chi-sq \n without this pattern",
                   main="dXsq by leverage")
    graphics::points(dx1[ ,lev], dx1[ ,dXsq], pch=pch, cex=cex,
           col=cols, bg=cols)
    if (identify) graphics::identify(dx1[ ,lev], dx1[ ,dXsq])
    maintext()
### dBhat
    graphics::plot(dx1[ ,lev], dx1[ ,dBhat],
                   xlab="Leverage for this covariate pattern",
                   ylab="dBhat = decrease in Bhat \n without this pattern",
                   main="dBhat by leverage")
    graphics::points(dx1[ ,lev], dx1[ ,dBhat], pch=pch, cex=cex,
           col=cols, bg=cols)
    if (identify) graphics::identify(dx1[ ,lev], dx1[ ,dBhat])
    maintext()
### dDev
    graphics::plot(dx1[ ,lev], dx1[ ,dDev],
                   xlab="Leverage for this covariate pattern",
                   ylab="dDev = decrease in Deviance \n without this pattern",
                   main="dDev by leverage")
    graphics::points(dx1[ ,lev], dx1[ ,dDev], pch=pch, cex=cex,
           col=cols, bg=cols)
    if (identify) graphics::identify(dx1[ ,lev], dx1[ ,dDev])
    maintext()
###
### ROC curve
### get name of y/ outcome variable from formula
    r1 <- pROC::roc(response=x$data[[ncol(x$data)]],
                    predictor=x$fitted,
                    ci=TRUE, percent=TRUE)
### 0.5 = chance, aim >0.7
    pROC::plot.roc(r1, print.auc=TRUE, grid=TRUE,
                   print.auc.cex=0.8, main="ROC curve")
    maintext()
### influence plot
    if (isTRUE(identify)){
        car::influencePlot(x,
                           id.n=1.5,
                           id.method="identify",
                           id.col="blue", scale=15,
                           xlab="Hat values (vertical lines at x2, x3 average hat value)",
                           main="Area proportional to Cooks distance",
                           sub="Cooks = change in coefficients if this point dropped")
    } else {
	car::influencePlot(x, id.n=1.5,
                           id.method="noteworthy",
                           id.col="blue", scale=15,
                           xlab="Hat values \n (vertical lines at *2, *3 average hat value)",
                           main="Area proportional to Cooks distance",
                           sub="Cooks = change in coefficients if this point dropped")
    }
    maintext()
###
###---------------------------------------------------------
### optional plots
###
    if (extras){
### studentized residuals vs hat values
        graphics::plot(rstudent(x) ~ hatvalues(x),
                       xlab="Hat values",
                       ylab="Studentized residuals \n (residual / residual std. dev.)",
                       main="Studentized residuals by hat values")
        graphics::points(hatvalues(x), rstudent(x), pch=pch, cex=cex,
                         col=cols, bg=cols)
        if (identify) graphics::identify(hatvalues(x), rstudent(x), cex=1.5)
        maintext()
### spread level plot
        car::spreadLevelPlot(x, layout=NA,
                             main="Spread-Level plot \n Spread should be even \n if variance of error is constant")
        maintext()
### qqnorm for residuals
        q1 <- stats::qqnorm(statmod::qresid(x),col="darkblue", cex=cex,
                            main="Normal Q-Q plot for residuals from formula \n Check if residuals normally distributed",
                            ylab="Quantiles from formula")
        stats::qqline((statmod::qresid(x)), col="darkblue", lwd=cex)
        if (identify) graphics::identify(q1, cex=cex)
        maintext()
### Cooks, stud~ res~ and hat values by index
        if (identify){
            car::influenceIndexPlot(x, col=cols[1],
                                    id.method = "identify",
                                    vars=c("Cook", "Studentized", "hat"),
                                    main="Cooks distance, studentized residuals \n and hat values by Index \n (of observations from original model data)")
        } else {
            car::influenceIndexPlot(x, col=cols[1],
                                    id.method = "y",
                                    vars=c("Cook", "Studentized", "hat"),
                                    main="Cooks distance, studentized residuals \n and hat values by Index \n (of observations from original model data)")
        }
### pairs plot for measures of influence
        graphics::pairs(dx1[ ,list(dBhat, dXsq, dDev)],
                        col=cols[1], cex=cex,
                        main="Pairs plot. Shows correlation between dBhat, dXsq, dDev")
### component + residual plots
        car::crPlots(x,
                     main="Partial (component +) residual plots. Check linear for each predictor.",
                     sub ="x[i]  vs.  b[i]*x[i] + residuals (from full model)")
### Av Plot
        car::avPlots(x, intercept=TRUE,
                     main="Added-variable plots = y ~ x[i], adjusted for other variables. Should be linear",
                     cex=cex)
### marginal model plots
### par( mfrow=c(nrow1, ncol1), oma = c(0,0,4,0), mar=c(4,6,3,0.5) )
### requires data.frame used to fit model be present in environment
### prevent stopping here due to error
        tryCatch(car::mmps(x), error=function(e)e)
        tex1 <- "Marginal model plots (with loess smooth): \n Plots of outcome for each and all predictors"
        graphics::mtext(tex1, line=0, outer = TRUE, cex=1.2)
    }
    par <- p
}
