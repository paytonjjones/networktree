#' Maximum Likelihood Estimation for Multivariate Normal Model
#'
#' Fit a multivariate normal model without covariates or covariance restrictions.
#' In addition to the (straightforward) parameter estimates the fitted log-likelihood
#' and corresponding score contributions are computed.
#'
#' Used internally in mobNetwork and networktreeModelBased
#'
#' @param y A matrix or data.frame where each row corresponds to a k-dim observation.
#' @param x Not used yet
#' @param start Not used yet
#' @param weights Not used yet
#' @param offset Not used yet
#' @param model Vector of characters. Specifies which estimated parameters are returned.
#' @param estfun Logical. Should the matrix of score contributions (aka estimating
#' functions) be returned?
#' @param object Not used yet
#' @param ... Not used yet
#'
#'@export
mvnfit <- function(y, x = NULL, start = NULL, weights = NULL,
                   offset = NULL, model = c("correlation", "mean", "variance"), ...,
                   estfun = FALSE, object = FALSE)
{
    # TODO:
    #  weights

    ### parameters
    y <- as.matrix(y)
    n <- nrow(y)
    k <- ncol(y)
    ynam <- if(is.null(colnames(y))) 1L:k else colnames(y)

    ### put dots in a list
    dotlist <- list(...)

    ### check if correlation matrix is identified
    if(n <= k*(k-1)/2) {
        stop("mvnfit: n < k*(k-1)/2, correlation matrix is not identified.")
    }

    ### MLE mu
    coef   <- colMeans(y)
    pnames <- paste0("mu_", ynam)

    ### MLE cov
    Sig <- cov(y) * (n - 1)/n
    coef <- c(coef, sqrt(diag(Sig)))
    pnames <- c(pnames, paste0("sigma_", ynam))

    ## MLE rho
    Om <- cov2cor(Sig)
    coef <- c(coef, Om[lower.tri(Om)])
    pnames <- c(pnames, paste0("rho_", ynam[which(lower.tri(Om), arr.ind = TRUE)[, 2L]],
                                  "_", ynam[which(lower.tri(Om), arr.ind = TRUE)[, 1L]]))
    names(coef) <- pnames

    ### compute loglik
    y <- (t(y) - coef[1L:k]) / coef[1L:k + k]           # scale y
    dec <- tryCatch(chol(Om), error = function(e) e)
    if(inherits(dec, "error")) {
        loglik <- Inf
    }
    else {
        tmp <- backsolve(dec, y, transpose = TRUE)
        loglik <- -n * (.5 * k * log(2*pi) + sum(log(coef[1L:k + k])) +
                  sum(log(diag(dec)))) - .5 * sum(tmp^2)
    }

    ### estfun
    ef <- NULL
    if(estfun & !inherits(dec, "error")) {
        ### invert Sigma
        InvOm <- chol2inv(dec)

        ### expand to length of y
        sigma_y <- rep.int(coef[1L:k + k], rep.int(n, k))

        ### re-transpose y
        y <- t(y)
        yy <- t(backsolve(dec, tmp))     ## eq. y %*% InvOm

        ### scores mu
	ef <- yy/sigma_y

        ### scores sigma
        ef <- cbind(ef, (y * yy - 1)/sigma_y)

        ### scores rho
        ef <- cbind(ef, combn(k, 2,
               function(x) (yy[,x[1]] * yy[,x[2]] - InvOm[x[2],x[1]])/2))

        colnames(ef) <- names(coef)
    }

    ### select requested parameters
    if(!is.null(dotlist$cor)) {
        model <- if(dotlist$cor) "correlation" else c("mean", "variance", "correlation")
    }
    id <- NULL
    if(any("mean"        == model)) id <- c(id, 1:k)
    if(any("variance"    == model)) id <- c(id, 1:k + k)
    if(any("correlation" == model)) id <- c(id, 1:(k*(k-1)/2) + 2*k)

    coef <- coef[id]
    ef   <- ef[, id]

#    ### estimate vcov based on OPG
    vc <- NULL
#    if(object & (length(coef) > n)) {
#        vc <- chol2inv(qr.R(qr(ef)))
#        rownames(vc) <- colnames(vc) <- colnames(ef)
#    }

    ### return
    list(coefficients = coef,
         objfun = -loglik,
         estfun = ef,
         object = vc)
}

