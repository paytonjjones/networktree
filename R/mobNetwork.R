#' Model-based recursive partitioning for networks
#'
#' This function is equivalent to networktreeModelBased, but using the
#' traditional partykit interface.
#'
#' Computes a tree model with networks as the end branches using model-based
#' recursive partitioning. Wraps the mob function from partykit.
#'
#' A high-level convenience interface to mob() + mvnfit()
#'
#' @param formula A symbolic description of the model to be fit. This
#' should either be of type \code{y1 + y2 + y3 ~ x1 + x2} with reponse
#' vectors \code{y1}, \code{y2}, and \code{y3} or \code{y ~ x1 + x2}
#' with a matrix response {y}. \code{x1} and \code{x2} are used as
#' partitioning variables.
#' @param data a data frame containing the variables in the model
#' @param na.action a function which indicates what should happen when the data
#' contain missing values (\code{NA}s).
#' @param splitBy if "network", splits only by the correlations between variables.
#' if "data", considers means and variances of each variable (i.e., mu and sigma)
#' @param ... arguments passed to \code{\link[partykit]{mob_control}}
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' d <- data.frame(trend = 1:200, foo = runif(200, -1, 1))
#' d <- cbind(d, rbind(
#'   rmvnorm(100, mean = c(0, 0, 0),
#'           sigma = matrix(c(1, 0.5, 0.5, 0.5, 1, 0.5, 0.5, 0.5, 1), ncol = 3)),
#'   rmvnorm(100, mean = c(0, 0, 0),
#'           sigma = matrix(c(1, 0, 0.5, 0, 1, 0.5, 0.5, 0.5, 1), ncol = 3))
#' ))
#' colnames(d)[3:5] <- paste0("y", 1:3)
#'
#' ## Now use the function
#' tree1 <- networktreeConditional(netdata=d[,3:5], splitvars=d[,1:2])
#' tree1 <- mobNetwork(formula = y1 + y2 + y3 ~ trend + foo, data=d)
#'
#'}
#' @return \code{\link{networktreeConditional}} returns a list of class "\code{}" which contains:
#'
#'@export
mobNetwork <- function(formula, data, na.action, splitBy ="network", ...)
{
  ## manage splitBy
  cor <- switch(splitBy,
                "network"=TRUE,
                "data"=FALSE)

  ## keep call
  cl <- match.call(expand.dots = TRUE)

  ## use dots for setting up mob_control
  control <- partykit::mob_control(...)
  control$ytype <- "matrix"

  ## control options for mvnfit
  mvncontrol <- list(cor = cor)

  ## call mob
  m <- match.call(expand.dots = FALSE)
  m$fit <- mvnfit
  m$control <- control
  for(n in names(mvncontrol)) if(!is.null(mvncontrol[[n]])) m[[n]] <- mvncontrol[[n]]
  if("..." %in% names(m)) m[["..."]] <- NULL
  m[[1L]] <- as.call(quote(partykit::mob))
  rval <- eval(m, parent.frame())

  ## extend class and keep original call
  rval$info$call <- cl
  class(rval) <- c("networktree", "modelbased", class(rval))
  return(rval)
}

