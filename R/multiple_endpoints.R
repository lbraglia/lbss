#' Double continuous primary outcome
#' 
#' A stripped down version of mpe::at.least.one.endpoint, focused on double
#' primary outcome, with starting sig_level = 0.025 by default (not 0.05, as done in
#' sozu) in order to be comparable to a two-tailed's 0.05
#' returns the number of patients per group: that is has to be multiplied by two
#'
#' @examples
#' 
#' ## example page 66 sozu, sugimoto, hamasaki evans (sample size determination
#' ## in clinical tirals with multiple endpoint)

#' me_atleastone_among_two(deltas = c(0.47, 0.48), corr = 0)   # 50 in the book
#' me_atleastone_among_two(deltas = c(0.47, 0.48), corr = 0.3) # 56 in the book
#' me_atleastone_among_two(deltas = c(0.47, 0.48), corr = 0.8) # 70 in the book
#' me_atleastone_among_two(deltas = c(0.47, 0.48), corr = 1)   # 83 in the book

#' ## table 5.1 pag 64 (for power = 0.8)
#' d1 <- d2 <- seq(0.2, 0.4, by = 0.05)
#' corrs <- c(0, 0.3, 0.5, 0.8, 1)
#' cases <- expand.grid("d1" = d1, "d2" = d2, "rho" = corrs)
#' cases <- cases[with(cases, d1 <= d2), ]
#' cases <- cases[with(cases, order(d1, d2, rho)), ]
#' cases$sample_size <- apply(cases, 1, function(row){
#'   me_atleastone_among_two(deltas = c(row[1], row[2]), corr = row[3])
#' })
#' tab51 <- reshape(cases, timevar = "rho", direction = "wide", idvar = c("d1", "d2"))
#' tab51$E1 <- apply(tab51, 1, function(row){
#'   round(power.t.test(delta = row[1], sig.level = 0.025 / 2, power = 0.8,
#'                      alternative = 'one.sided')$n)
#' })
#' tab51$E2 <- apply(tab51, 1, function(row){
#'   round(power.t.test(delta = row[2], sig.level = 0.025 / 2, power = 0.8,
#'                      alternative = 'one.sided')$n)
#' })
#' tab51
#'
#'
#'@export
me_atleastone_among_two <- function(deltas = c(0.2, 0.2),
                                    sds = c(1, 1),
                                    corr = 0.5,
                                    sig_level = 0.025 / length(deltas),
                                    power = 0.8){
  
  std_effects <- deltas / sds
  cov <- corr * prod(sds)
  varcov_matrix <- diag(sds^2) * (1 - cov) + cov
  z_alpha <- qnorm(1 - sig_level)
  
  power_atleastone_among_two <- function(n, std.effect, z.alpha, sigma, power){
    ## this function returns the difference between study power and desidered
    ## power is minimized for n
    crit_vals <- z.alpha - sqrt(n / 2) * std.effect
    ## equation pag 60 of sozu
    (1 - mvtnorm::pmvnorm(upper = crit_vals, sigma = sigma)) - power
  }
  
  n <- uniroot(power_atleastone_among_two,
               interval = c(2, 1e+05),         # optimize between n=2 and n=10000
               tol = .Machine$double.eps^0.25, # as in mpe::at.least.one.endpoint
               extendInt = "yes",              # as in mpe::at.least.one.endpoint
               std.effect = std_effects,       # function parameters
               z.alpha = z_alpha,
               sigma = varcov_matrix,
               power = power)$root
  ceiling(n)
}


#' Double continuous co-primary outcomes
#'
#' 
#' @examples
#' ## example page pag 20 sozu, sugimoto, hamasaki evans (sample size determination
#' ## in clinical tirals with multiple endpoint)
#' me_both_the_two(deltas = c(0.47, 0.48), corr = 0)   # 92 in the book
#' me_both_the_two(deltas = c(0.47, 0.48), corr = 0.3) # 90 in the book
#' me_both_the_two(deltas = c(0.47, 0.48), corr = 0.5) # 87 in the book
#' me_both_the_two(deltas = c(0.47, 0.48), corr = 0.8) # 82 in the book
#' 
#' ## table 2.1 pag 14 (for power = 0.8)
#' d1 <- d2 <- seq(0.2, 0.4, by = 0.05)
#' corrs <- c(0, 0.3, 0.5, 0.8, 1)
#' cases <- expand.grid("d1" = d1, "d2" = d2, "rho" = corrs)
#' cases <- cases[with(cases, d1 <= d2), ]
#' cases <- cases[with(cases, order(d1, d2, rho)), ]
#' cases$sample_size <- apply(cases, 1, function(row){
#'   me_both_the_two(deltas = c(row[1], row[2]), corr = row[3])
#' })
#' tab21 <- reshape(cases, timevar = "rho", direction = "wide", idvar = c("d1", "d2"))
#' tab21$E1 <- apply(tab21, 1, function(row){
#'   round(power.t.test(delta = row[1], sig.level = 0.025, power = 0.8,
#'                        alternative = 'one.sided')$n)
#' })
#' tab21$E2 <- apply(tab21, 1, function(row){
#'   round(power.t.test(delta = row[2], sig.level = 0.025, power = 0.8,
#'                        alternative = 'one.sided')$n)
#' })
#' 
#' tab21
#' 
#'
#' @export
me_both_the_two <- function(deltas = c(0.2, 0.2),
                            sds = c(1, 1),
                            corr = 0.5,
                            sig_level = 0.025,
                            power = 0.8){
  
  std_effects <- deltas / sds
  cov <- corr * prod(sds)
  varcov_matrix <- diag(sds^2) * (1 - cov) + cov
  z_alpha <- qnorm(1 - sig_level)
  
  power_both_the_two <- function(n, std.effect, z.alpha, sigma, power){
    ## this function returns the difference between study power and desidered
    ## power is minimized for n
    crit_vals <- z.alpha - sqrt(n / 2) * std.effect
    ## equation 2.2 pag 8 of sozu
    mvtnorm::pmvnorm(lower = crit_vals, sigma = sigma) - power
  }
  
  n <- uniroot(power_both_the_two,
               interval = c(2, 1e+05),         # optimize between n=2 and n=10000
               tol = .Machine$double.eps^0.25, # as in mpe::at.least.one.endpoint
               extendInt = "yes",              # as in mpe::at.least.one.endpoint
               std.effect = std_effects,       # function parameters
               z.alpha = z_alpha,
               sigma = varcov_matrix,
               power = power)$root
  ceiling(n)
}
