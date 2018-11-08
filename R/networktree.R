#' Tree model (partitioning) with networks at the end of branches
#'
#' Computes a tree model with networks at the end of branches. Can use
#' model-based recursive partitioning (see networktreeModelBased) or
#' conditional inference (see networktreeConditional).
#'
#' Wraps the mob() and ctree() functions from the partykit package.
#'
#' For interfaces more consistent with the partykit standard, see
#' the mobNetwork() and ctreeNetwork() functions
#'
#' @param data the cross-sectional dataset from which to compute the network
#' @param splitVars the variables with which to test split the network. Can be vector, matrix, or dataframe
#' @param type the type of network to compute. Can be "cor", "pcor", or "EBICglasso"
#' @param method "ModelBased" or "Conditional"
#' @param weights weights
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
#' tree1 <- networktree(netdata=d[,3:5], splitVars=d[,1:2])
#'
#'}
#' @return \code{\link{networktree}} returns a list of class "\code{}" which contains:
#'
#'@export
networktree <- function(data, splitVars, type=c("cor", "pcor", "EBICglasso"),
                                   method=c("ModelBased","Conditional"),
                                   weights=NULL,...){
  if(method[1]=="ModelBased"){
    res <- networktreeModelBased(data, splitVars, type=type,...)
  } else if(method[1]=="Conditional"){
    res <- networktreeConditional(data, splitVars, type=type,
                                  weights=NULL,...)
  }
  return(res)
}

## methods
print.networktree<- function(x,
                             title = "Network tree", objfun = "negative log-likelihood", ...)
{
  partykit::print.modelparty(x, title = title, objfun = objfun, ...)
}

#' Plotting 'treenetwork' objects
#'
#' Wraps plot.party to plot a tree model with networks on the ends. Networks
#' are plotted with qgraph, and additional arguments are passed there
#'
#' @param x an object of type 'networktree'
#' @param type "cor", "pcor", or "EBICglasso". If set to NULL, type detected from x
#' @param layout argument passed to qgraph. Can be set to custom layout.
#' @param ... additional arguments passed qgraph
#'
#'@export
plot.networktree <- function(x, type=NULL,layout="circle", ...) {
  
  if(is.null(type)) {
    type <- if("cor" %in% class(x)) {"cor"} else if ("pcor" %in% class(x)) {"pcor"
    } else if("EBICglasso" %in% class(x)) {"EBICglasso"
    } else {"EBICglasso";
      warning("Type of network could not be detected, plotting EBICglasso networks")}
  }

  net_terminal_inner <- function(obj, ...) {
    net_terminal(obj,type=type,layout=layout, ...)
  }
  class(net_terminal_inner) <- "grapcon_generator"
  partykit::plot.party(x, terminal_panel=net_terminal_inner,...)
}

