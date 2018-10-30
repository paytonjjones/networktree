#' Conditional inference trees for networks
#'
#' Computes a tree model with networks as the end branches using conditional inference.
#' Wraps the ctree function from partykit. This approach is slow; for a faster method
#' see networkModelBased.
#'
#' @param data the cross-sectional dataset from which to compute the network
#' @param splitVars the variables with which to test network splitting. Can be vector, matrix, or dataframe
#' @param weights weights
#' @param type the type of network to compute. Can be "cor", "pcor", or "EBICglasso"
#' @param na.action a function which indicates what should happen when the data
#' contain missing values (\code{NA}s).
#' @param ... additional arguments passed cortrafo
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
#' tree1 <- networktreeConditional(netdata=d[,3:5], splitVars=d[,1:2])
#'
#'}
#' @return \code{\link{networktreeConditional}} returns a list of class "\code{}" which contains:
#'
#'@export
networktreeConditional <- function(data, splitVars, weights=NULL, type=c("cor", "pcor", "EBICglasso"),
                                   na.action,...){
  netdata <- as.data.frame(data); splitVars <- as.data.frame(splitVars)
  if(is.null(colnames(netdata))){colnames(netdata) <- paste("var",1:ncol(netdata), sep="")}
  if(is.null(colnames(splitVars))){colnames(splitVars) <- paste("s.var",1:ncol(netdata), sep="")}
  d <- cbind(netdata, splitVars)
  f1 <- Formula::as.Formula(paste(c(paste(colnames(netdata),collapse=" + "), " ~ ", paste(colnames(splitVars), collapse=" + ")), collapse=""))
  n <- ncol(netdata)
  control<-NULL
  # Need to include n so cortrafo can count vars on left hand side
  tree <- partykit::ctree(formula=f1, data=d,
                          ytrafo=function(data, weights,control,...) {cortrafo(data=data, weights=weights, control=control, n=n,...)},
                          weights, ...)
  class(tree) <- c("networktree", "conditional", type[1], class(tree))
  return(tree)
}
