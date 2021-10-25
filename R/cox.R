## formula implementata: hsieh e lavori 2000 che assumono una
## distribuzione normale dello stimatore
#' @export
cox_n <- function(alpha = 0.05, beta = 0.2, p = 1, loghr, r2, stdev){
    n <- ((qnorm(1 - alpha/2) + qnorm(1 - beta))^2)/
        (p*(1 - r2)*(stdev^2)*(loghr^2))
    data.frame(alpha = alpha,
               beta = beta,
               p = p,
               loghr = loghr,
               r2 = r2,
               stdev = stdev,
               n = n)
}


# per ottenere il loghr che riusciamo a dimostrare con un dato n
# con semplici passaggi algebrici si ottiene
cox_loghr <- function(alpha, beta, p, n, r2, stdev){
    sqrt(
    ((qnorm(1 - alpha/2) + qnorm(1 - beta))^2)/
    (p*(1 - r2)*(stdev^2)*(n))
    )
    data.frame(alpha = alpha,
               beta = beta,
               p = p,
               n = n,
               r2 = r2,
               stdev = stdev,
               loghr = loghr
               )

}
