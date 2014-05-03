##' @name logiDx
##' @export
##' @title Diagnostics for logistic regression
##' @include genLogi.R
##' @description
##' Returns standard diagnostic measures for a logistic regression model
##' by covariate pattern
##' @param x A model of class \code{glm}
##' @param round If \code{round=TRUE}, digits will be ronded to
##' \code{roundTo} decimal places
##' @param roundTo No. decimal places to which to round digits
##' @return A \code{data.table}. There is one row per covariate pattern
##' with at least one observation.
##' These are sorted by \code{dBhat} (see below).
##' \cr \cr
##' The initial columns give all combinations of the predictor
##' variables with at least one observation.
##' \cr \cr
##' Subsequent columns are labelled as follows:
##' \item{obs}{Number of observations with this covariate pattern}
##' \item{prob}{Probability of this covariate pattern}
##' \item{yhat}{Number of observations of \eqn{y=1}, \emph{predicted} by the model}
##' \item{y}{\emph{Actual} number of observations of
##' \eqn{y=1} from the data}
##' \item{lev}{\dfn{Leverage}, the diagonal of the hat matrix used to
##' generate the model; a measure of influence of this covariate pattern}
##' \item{devR}{\dfn{Deviance residual}, calculated by covariate pattern; a
##' measure of influence of this covariate pattern}
##' \item{PeR}{\dfn{Pearson residual}, calculated by covariate pattern; a
##' measure of influence of this covariate pattern. Given by:
##' \deqn{ \sqrt{obs}\sqrt{\frac{prob}{(1-prob)}}}{
##'  obs^0.5 (prob/1-prob)^0.5}}
##' \item{sPeR}{\dfn{Standardized Pearson residual} calculated by covariate
##' pattern; a measure of influence of this covariate pattern. Given by:
##' \deqn{ \frac{PeR}{\sqrt{(1-lev)}}}{
##'  PeR.(1-lev)^0.5}}
##' \item{dBhat}{\dfn{Change in Bhat}, the standardized difference between
##' the original maximum likelihood estimates \bold{B} and that estimates
##' with this covariate pattern excluded}
##' \item{dXsq}{\dfn{Change in chi-square}, decrease in the value of
##' Pearson chi-square statistic with this covariate pattern excluded. Given by:
##' \deqn{sPeR^2}}
##' \item{dDev}{\dfn{Change in deviance} \bold{D} with this covariate
##' pattern excluded. Given by:
##' \deqn{ \frac{dev^2}{(1-lev)}}{
##' d^2/(1-lev)}}
##' @note Values for the statistics are calculated by \emph{covariate pattern}.
##' Different values may be obtained if calculated for each individual
##' obervation (i.e. row in data frame).
##' \cr \cr
##' Generally, the values calculated by covariate pattern are preferred,
##' particularly where no. observations are \eqn{>5}.
##' @seealso \code{\link{plotLogiDx}}
##' @examples
##' d1 <- genLogiDt(model=FALSE)
##' f1 <- stats::glm(y ~ I(x5^2)*x1 -1, family=binomial("logit"), data=d1)
##' logiDx(f1)
##'
logiDx <-
function(x, round=FALSE, roundTo=3){
    stopifnot(inherits(x, "glm"))
    obs <- prob <- PeR <- lev <- sPeR <- devR <- dBhat <- PeR <- devR <- NULL
    yhat <- y <- yhatY1 <- y0 <- yhatY0 <- NULL
    v <- x$coefficients
### get model matrix
    x1 <- model.matrix(x)
### intercept present?
    intP <- "(Intercept)" %in% names(v)
    if (intP) {
        x1 <- x1[, -1]
        int1 <- v[1]
        v <- v[-1]
    }
### length coefficients (not intercept)
    l1 <- dim(x1)[2]
### unique co-variate patterns
    u1 <- unique(x1)
    res1 <- data.table(u1)
### nrows in result
    nr1 <- dim(res1)[1]
### no. observations per covariate pattern
### string expression:
### subset model matrix by covariate pattern
    subsetByCov <- function(j) paste0(
        "x1[,", j, "]==u1[i,", j, "]")
    e1t <- sapply(1:l1, subsetByCov)
    e1 <- paste(e1t, collapse=" & ")
### convert string to expression
    e1 <- parse(text=e1)
    subsetByExpr <- function(i) nrow(subset(x1, eval(e1)))
### obs is no observations fitting criteria
    res1[, "obs" := sapply(seq.int(nr1), subsetByExpr)]
### probability (predicted) each covariate pattern
### creates expression for each unique subset
    genLogit <- function(j) paste0(
        "v[", j, "] * x1[i,", j, "]")
    e1t <- sapply(1:l1, genLogit)
    e1 <- paste(e1t, collapse=" + ")
    if(intP) e1 <- paste(int1, e1, sep=" + ")
    e1 <- parse(text=e1)
### eval(e1) gives logit for this covariate pattern
    logitToProb <- function(i) exp(eval(e1)) / (1+exp(eval(e1)))
### prob is predicted probability for each pattern
    res1[, "prob" := sapply(seq.int(nr1), logitToProb)]
###
### yhat =  predicted 'no. outcome=1' in each pattern
### yhat = obs * prob
    res1[, "yhat" := obs*prob]
### acutal no.
    e1 <- sapply(1:l1, subsetByCov)
    e1 <- paste(e1, collapse=" & ")
    e1 <- parse(text=e1)
    y1 <- model.response(model.frame(x))
### actual no observations fitting these criteria:
    totalY <- function(i) sum(y1[which(eval(e1))] == 1)
    res1[, "y" := sapply(seq.int(nr1), FUN=totalY)]
###
###--------------------------------
### make hat matrix
###
### add intercept term to make Design matrix if necessary
    if(intP) u1 <- cbind(rep(1,nrow(u1)), u1)
### no. observations fitting criteria
    v <- res1[ , obs] * res1[, prob] * (1-(res1[, prob] ))
### convert to diagonal matrix and take square root
    v1 <- diag(v)
    v1s <- sqrt(v1)
### hat matrix
    H1 <- v1s %*% u1 %*% solve(t(u1) %*% v1 %*% u1) %*% t(u1) %*% v1s
### hat diagonals = leverage
    res1[, "lev" := diag(H1)]
###
### deviance residual by covariate pattern
    devByCov <- function(j){
### y=0 for this covariate pattern
	if (res1[j, y] ==0){
            d1 <- log( (1-res1[j, prob]) )
            dev <- -sqrt(2 * res1[j, obs] * abs(d1))
            return(dev)
### y = no. obs. for this covariate pattern
        } else if (res1[j, y] == res1[j, obs]){
            d1 <- log( (res1[j, prob]) )
            dev <- sqrt( 2 * res1[j, obs] * abs(d1))
            return(dev)
        } else {
            d1 <- res1[j, y] / (res1[j, yhat])
            d2 <- res1[j, y] * log(d1)
            d3 <- ( res1[j, obs] - res1[j, y] ) / ( res1[j, obs] * (1 -res1[j, prob]) )
            d4 <- (res1[j, obs] - res1[j, y]) * log(d3)
            d5 <- sqrt( 2*(d2 + d4) )
### 1 if +ve, 0 if -ve
            s1 <- sign(res1[j, y] - res1[j, yhat])
            dev <- s1*d5
            return(dev)
	}
    }
    res1[, "devR" := sapply(seq.int(nr1), devByCov)]
###
### Pearson residual
    Pear <- function(j){
### y=0 for this covariate pattern
	if (res1[j, y] ==0){
            Pr1 <- sqrt( res1[j, prob] / (1-res1[j, prob]))
            Pr2 <- -sqrt (res1[j, obs])
            res <- Pr1 * Pr2
            return(res)
        } else {
### y>0 for this covariate pattern
            Pr1 <- res1[j, y] - res1[j, yhat]
            Pr2 <- sqrt(res1[j, yhat] * ( 1-(res1[j, prob])))
            res <- Pr1/Pr2
            return(res)
	}
}
    res1[, "PeR" := sapply(seq.int(nr1), FUN=Pear)]
###

### Standardized Pearson residuals
    res1[, "sPeR" := PeR / (sqrt(1 - lev))]
###
### dBhat
### standardized difference in B (maximum likelihood coefficients
### for paramaters) without this pattern
### (standardized by covariance matrix of B);
### should be <1 if little influence on model
###
    res1[, "dBhat" := sPeR^2 * lev / (1 - lev)]
###
### dXsq
### decrease in Pearson chi-square without this pattern,
### should be <4 if little influence on model
    res1[, "dXsq" := sPeR^2]
### dDev
### decrease in deviance without this pattern
    res1[, "dDev" := devR^2 / (1 - lev)]
    setkey(res1, dBhat)
    if (round) round(res1, roundTo)
###
### class(res) <- c("logisticDx","matrix")
    return(res1)
}
