#' Conditional inference trees for networks
#'
#' This function is equivalent to networktreeConditional, but using the
#' traditional partykit interface.
#'
#' Computes a tree model with networks as the end branches using conditional inference.
#' Wraps the ctree function from partykit.
#'
#' A high-level convenience interface to ctree() + mvnfit()
#'
#' @param formula A symbolic description of the model to be fit. This
#' should either be of type \code{y1 + y2 + y3 ~ x1 + x2} with reponse
#' vectors \code{y1}, \code{y2}, and \code{y3} or \code{y ~ x1 + x2}
#' with a matrix response {y}. \code{x1} and \code{x2} are used as
#' partitioning variables.
#' @param data a data frame containing the variables in the model
#' @param weights weights
#' @param type the type of network to compute. Can be "cor", "pcor", or "EBICglasso"
#' @param na.action a function which indicates what should happen when the data
#' contain missing values (\code{NA}s).
#' @param ... arguments passed to \code{\link[partykit]{mob_control}}
#'
#' @return \code{\link{ctreeNetwork}} returns a list of class "\code{}" which contains:
#'
#'@export
ctreeNetwork <- function(formula, data, weights=NULL, type=c("cor", "pcor", "EBICglasso"), 
                         na.action, ...)
{
  charformulaLHS <-   strsplit(as.character(formula), "+", fixed=T)[[2]]
  n <- length(charformulaLHS)
  control<-NULL
  # Need to include n so cortrafo can count vars on left hand side
  tree <- partykit::ctree(formula=formula, data=data,
                          ytrafo=function(data, weights,control,...) {cortrafo(data=data, weights=weights, control=control, n=n,...)},
                          weights, ...)
  class(tree) <- c("networktree", "conditional", type[1], class(tree))
  return(tree)
}
