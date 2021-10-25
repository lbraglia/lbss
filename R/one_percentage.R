#' Wilson confidence interval width 
#'
#' Wilson confidence interval width for a given sample size and, alpha and p
#' @param n sample size
#' @param alpha alpha
#' @param p expected proportion
#' @export
wilson_width <- function(n, alpha = 0.05, p = 0.5){
    ## intervallo a due code
    z <- qnorm(1 - alpha/2)
    num1 <- 2 * n * p + z^2
    num2 <- z * sqrt(z^2 + 4 * n * p* (1-p))
    denominator <- 2* (n + z^2)
    lower <- (num1 - num2)/denominator
    upper <- (num1 + num2)/denominator
    data.frame(confidence.lev = 1 - alpha,
               n = n,
               p = p,
               lower = lower,
               upper = upper,
               width = upper - lower,
               half_width = (upper - lower)/2)
}


wilson_n <- function(width, alpha = 0.05, p = 0.5){

}



## #' Clopper-Pearson confidence interval width 
## #'
## #' Clopper-Pearson confidence interval width for a given sample size
## #' and, alpha and p
## #' 
## #' @param n sample size
## #' @param alpha alpha
## #' @param p expected proportion

## cp_width <- function(n, alpha = 0.05, p = 0.5){

## }


## cp_n <- function(width, alpha = 0.05, p = 0.5){

## }
