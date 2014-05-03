##' @name logiProb
##' @export
##' @include genLogi.R
##' @title Logit, odds ratio and probability for coefficients of a
##' logistic regression
##' @description
##' Generate logit, log odds ratio, odds ratio and probability for
##' coefficients in a logistic regression.
##' Can be generalized to all combinations of coefficients.
##' \cr \cr
##' Values are calculated for a change in the value of the coeffient for the
##' predictor from \eqn{0} to \eqn{1}. (For continuous predictors changes of more than one
##' unit may have more practical significance).
##'
##' @param x A logistic regression model of class \code{glm}
##' @param all If \code{all=FALSE} (the default) return values for all
##' coefficients in model, considered together.
##' \cr
##' If \code{all=TRUE} return values for all \emph{combinations} of
##' coefficients in model.
##' @param usePrim If \code{usePrim=FALSE} (the default) use \code{utils::combn}
##' to generate combinations.
##' \cr
##' If \code{usePrim=TRUE} use
##' \code{gRbase::combnPrim} instead (faster).
##' @return If \code{all=TRUE}, a \code{data.table} giving,
##' for each \emph{combination} of coefficients:
##' \item{coef}{Combination of coefficients}
##' \item{logit}{The logit for a given combination of coefficients}
##' \item{lnOR}{Natural log of Odds Ratio}
##' \item{OR}{Odds Ratio}
##' \item{p}{probability}
##' This is sorted by \code{OR} (low to high).
##' \cr \cr
##' If \code{all=FALSE}, a \code{data.frame} giving the above values
##' for all predictors.
##' @note To use \code{gRbase::combnPrim} the following dependencies
##' may be necessary. Install as follows:
##' \cr
##' source("http://bioconductor.org/biocLite.R") \cr
##' biocLite("graph") \cr
##' biocLite("BiocGenerics") \cr
##' biocLite("RBGL") \cr
##' @examples
##' set.seed(1)
##' f1 <- genLogiDf(n=50)$model
##' logiProb(f1)
##' d1 <- genLogiDt(n=50, model=FALSE)
##' logiProb(glm(y ~ x1 + x3 -1, data=d1, family=binomial()), all=TRUE)
##'
logiProb <- function(x, usePrim=FALSE, all=FALSE){
    stopifnot(inherits(x, "glm"))
    prob <- NULL
###
    c1 <- x$coefficients
### intercept present
    int1 <- "(Intercept)" %in% names(c1)
    if (int1) names(c1)[1] <- "Int"
###
    if (!all){
        s1 <- sum(c1)
        prob1 <- exp(s1) / (1+exp(s1))
        res1 <- data.frame(
            "coef" = paste(names(c1), collapse="+"),
            "logit" = s1,
            "OR" = prob1 / (1-prob1),
            "lnOR" = log( prob1 / (1-prob1)),
            "prob" = prob1
            )
        return(res1)
    }
    if(usePrim){
        combo <- gRbase::combnPrim
    } else {
        combo <- utils::combn
    }
### binomial expansion; gives no. of logits
    ch1 <- choose(n=length(c1), k=seq.int(c1))
### names
### generate combinations of coefficient names
    .genComCoefName <- function(i){
### all combinations of coefficients with 'i' elments
        co1 <- combo(names(c1), i)
### change to character vector form
        pasteAddCol <- function(j) paste(co1[, j], collapse="+")
        co1 <- sapply(seq.int(dim(co1)[2]),
                      FUN=pasteAddCol, USE.NAMES=FALSE)
        return(co1)
        }
### predictors
    res1 <- data.table::data.table("coef" = unlist(sapply(seq.int(ch1),
                                   FUN=.genComCoefName)))
###
### generate logits (from combinations of coefficients)
    .genLogit <- function(i){
        co1 <- combo(c1, i)
        return(colSums(co1))
    }
### logits
    res1[, "logit" := unlist(sapply(seq.int(ch1), FUN=.genLogit))]
### convert logits to probabilities
### logitToProb <- function(i) exp(i)/ ( 1+exp(i) )
    res1[, "prob" := exp(logit) / (1+exp(logit))]
### convert probabilities to Odds Ratios
### probToOR <- function(p) p/(1-p)
    res1[, "OR" := prob / (1-prob)]
### convert probabilities to log of Odds Ratios
### probToLnOR <- function (p) log( p/(1-p))
    res1[, "lnOR" := log(prob / (1-prob))]
    setcolorder(res1, c(1, 2, 4, 5, 3))
    setkey(res1, "OR")
###
    return(res1)
}
###----------------------------------------
###
### library(gRbase)
### for gRbase::combnPrim instead of utils::combn (works faster)
### the following dependencies may be necessary, install as follows:
### source("http://bioconductor.org/biocLite.R")
### biocLite("graph")
### biocLite("BiocGenerics")
### biocLite("RBGL")
### ip1 <- rownames(installed.packages())
### (all(c("gRbase", "RBGL", "BiocGenerics") %in% ip1))

