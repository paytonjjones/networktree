#' Model-based recursive partitioning for networks
#'
#' Computes a tree model with networks as the end branches using model-based
#' recursive partitioning. Wraps the mob function from partykit.
#' See also networktreeConditional
#'
#' @param nodeVars the cross-sectional dataset from which to compute the network
#' @param splitVars the variables with which to test network splitting. Can be vector, matrix, or dataframe
#' @param type the type of network to compute. Can be "cor", "pcor", or "EBICglasso"
#' @param na.action a function which indicates what should happen when the data
#' contain missing values (\code{NA}s).
#' @param splitBy if "network", splits only by the correlations between variables.
#' if "data", considers means and variances of each variable (i.e., mu and sigma)
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
#' tree1 <- networktreeModelBased(nodeVars=d[,3:5], splitvars=d[,1:2])
#'
#'}
#' @return \code{\link{networktreeModelBased}} returns a list of class "\code{}" which contains:
#'
#'@export
networktreeModelBased <- function(nodeVars, splitVars, type=c("cor", "pcor", "EBICglasso"),
                                  na.action, splitBy="network",...){
  if(is.null(colnames(nodeVars))){colnames(nodeVars) <- paste('nodeVars',1:ncol(nodeVars))}
  if(is.null(colnames(splitVars))){colnames(splitVars) <- paste('splitVars',1:ncol(splitVars))}

  d <- cbind(nodeVars,splitVars)
  form <- paste(paste(colnames(nodeVars), collapse=" + "), "~",paste(colnames(splitVars), collapse=" + "))

  tr <- mobNetwork(form, data = d, na.action=na.action, splitBy = splitBy, type=type, ...)
  return(tr)
}
