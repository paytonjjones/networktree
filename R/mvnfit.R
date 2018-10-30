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
#' @param cor Logical. Should only be the parameter estimates and scores pertaining
#' to the correlations be returned (i.e., those for mu and sigma omitted)?
#' @param estfun Logical. Should the matrix of score contributions (aka estimating
#' functions) be returned?
#' @param object Not used yet
#' @param ... Not used yet
#'
#'@export
mvnfit <- function(y, x = NULL, start = NULL, weights = NULL,
                   offset = NULL, cor = FALSE, ...,
                   estfun = FALSE, object = FALSE)
{
    # TODO:
    #  weights

    ### parameters
    y <- as.matrix(y)
    n <- nrow(y)
    k <- ncol(y)
    ynam <- if(is.null(colnames(y))) 1L:k else colnames(y)

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

    ### skip mu and sigma results if requested
    if(cor) {
        coef <- coef[1:(k*(k-1)/2) + 2*k]
        ef   <- ef[, 1:(k*(k-1)/2) + 2*k]
    }

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

