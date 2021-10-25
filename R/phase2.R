#' Function to implement exact single-stage phase II design a-la AHern
#'
#' @param p0 response probability used in the definition of the null hypothesis
#' @param p1 desirable response probability at which the trial is powered
#' @param alpha type I error (default 0.05)
#' @param beta type II error  (default 0.2)
#' @param sample_sizes checked sample sizes, should be enough for most
#'     situations
#' 
#' @references A'Hern, Sample size tables for exact single-stage phase
#'     II designs, Statist. Med. 2001; 20:859–866
#' 
#' @examples
#' # simple example
#' ahern(p0 = 0.05, p1 = 0.1)
#' 
#' # A'Hern's number validation
#' probs <- seq(0.05, 0.95, by = 0.05)
#' analyses <- expand.grid(p0 = probs, p1 = probs,
#'                         alpha = c(0.05, 0.01), beta = c(0.2, 0.1))
#' select <- with(analyses, p1 > p0)
#' analyses <- analyses[select, ]
#' ord <- with(analyses, order(p0, p1, -alpha, -beta))
#' analyses <- analyses[ord, ]
#' rownames(analyses) <- NULL
#' res <- apply(analyses, 1, function(x)
#'     ahern(p0 = x[1], p1 = x[2], alpha = x[3], beta = x[4]))
#' res <- do.call(rbind, res)
#' rownames(res) <- NULL
#' res$pp <- with(res, paste(cutoffs, sample_size, sep = '/'))
#' res$sample_size <- NULL
#' res$cutoffs <- NULL
#' res_spl <- split(res, res$p0)
#' ## sink('ahern_validation.txt')
#' ## res_spl
#' ## sink()
#' 
#'@export
ahern <- function(p0, p1, alpha = 0.05, beta = 0.2, sample_sizes = 1:2000){

    if (p0 >= p1) stop("p0 must be < p1")
    ## restituisce il cutoff di eventi che rispetta le diverse ipotesi
    ## dato un determinato sample size
    worker <- function(p0, p1, a, b, n){
        evs = 0:n
        ## area sotto la binomiale, parte superiore sotto H0, inferiore sotto H1
        upper_probs_p0 <- pbinom(evs, n, p = p0, lower.tail = FALSE)
        lower_probs_p1 <- pbinom(evs, n, p = p1, lower.tail = TRUE)
        ## verifica che il cutoff rispetti entrambi i criteri
        scelti <- which(upper_probs_p0 < a & lower_probs_p1 < b)
        ## restituzione dei cutoff (al quale si aggiunge 1 per coerenza col
        ## paper e affinché tornino gli intervalli)
        if (length(scelti) == 0)
            return(NA)
        else
            return(evs[scelti][1] + 1)
    }
    ## while: stoppa al primo trial individuato che rispetti i requirements
    for (ss in sample_sizes){
        res <- worker(p0 = p0, p1 = p1, a = alpha, b = beta, n = ss)
        if (!is.na(res)) break
    }

    ## calcola il lower.ci dell'intervallo di clopper pearson sotto le ipotesi
    ## ed effettua checks
    ci <- binom.test(res, ss, alternative = 'greater', conf.level = 1 - alpha)
    lower_ci <- ci$conf.int[1]
    check <- lower_ci > p0
    data.frame(p0, p1, alpha, beta, sample_size = ss, cutoffs = res,
               lower_ci = lower_ci, lower_ci_gt_p0 = check)
}
