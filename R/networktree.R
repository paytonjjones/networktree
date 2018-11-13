
# S3 generic

#' Tree model (partitioning) with networks at the end of branches
#'
#' Computes a tree model with networks at the end of branches. Can use
#' model-based recursive partitioning or conditional inference.
#'
#' Wraps the mob() and ctree() functions from the partykit package.
#'
#' Default S3 method shown here. For a formula interface, see ?networktree.formula
#'
#' @param nodeVars the variables with which to compute the network. Can be vector, matrix, or dataframe
#' @param splitVars the variables with which to test split the network. Can be vector, matrix, or dataframe
#' @param type the type of network to compute. Can be "cor", "pcor", or "EBICglasso". Note that networks
#' are always stored internally as correlation matrices, but will be auto-adjusted in plots etc. according
#' to type
#' @param method "ModelBased" or "Conditional"
#' @param splitBy if "network", splits only by the correlations between variables.
#' if "data", considers means and variances of each variable (i.e., mu and sigma).
#' Available for method="ModelBased" only.
#' @param na.action a function which indicates what should happen when the data
#' contain missing values (\code{NA}s).
#' @param weights weights
#' @param ... additional arguments passed to mob_control (ModelBased) or cortrafo (Conditional)
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
#' tree1 <- networktree(nodeVars=d[,3:5], splitVars=d[,1:2])
#'
#'}
#'@export
networktree <- function(...) UseMethod("networktree")

#' Tree model (partitioning) with networks at the end of branches
#'
#' Computes a tree model with networks at the end of branches. Can use
#' model-based recursive partitioning or conditional inference.
#'
#' Wraps the mob() and ctree() functions from the partykit package.
#'
#' Default S3 method shown here. For a formula interface, see ?networktree.formula
#'
#' @param nodeVars the variables with which to compute the network. Can be vector, matrix, or dataframe
#' @param splitVars the variables with which to test split the network. Can be vector, matrix, or dataframe
#' @param type the type of network to compute. Can be "cor", "pcor", or "EBICglasso". Note that networks
#' are always stored internally as correlation matrices, but will be auto-adjusted in plots etc. according
#' to type
#' @param method "ModelBased" or "Conditional"
#' @param splitBy if "network", splits only by the correlations between variables.
#' if "data", considers means and variances of each variable (i.e., mu and sigma).
#' Available for method="ModelBased" only.
#' @param na.action a function which indicates what should happen when the data
#' contain missing values (\code{NA}s).
#' @param weights weights
#' @param ... additional arguments passed to mob_control (ModelBased) or cortrafo (Conditional)
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
#' tree1 <- networktree(nodeVars=d[,3:5], splitVars=d[,1:2])
#'
#'}
#'@export
networktree.default <- function(nodeVars, splitVars, type=c("cor", "pcor", "EBICglasso"),
                                   method=c("ModelBased","Conditional"),
                                   splitBy ="network",
                                   na.action,
                                   weights=NULL,...){
  if(method[1]=="ModelBased"){
    if(is.null(colnames(nodeVars))){colnames(nodeVars) <- paste('nodeVars',1:ncol(nodeVars))}
    if(is.null(colnames(splitVars))){colnames(splitVars) <- paste('splitVars',1:ncol(splitVars))}
    
    d <- cbind(nodeVars,splitVars)
    form <- paste(paste(colnames(nodeVars), collapse=" + "), "~",paste(colnames(splitVars), collapse=" + "))
    res <- networktree.formula(form, data = d, type=type, method=method, na.action=na.action, splitBy = splitBy, ...)
  } else if(method[1]=="Conditional"){
    netdata <- as.data.frame(nodeVars); splitVars <- as.data.frame(splitVars)
    if(is.null(colnames(netdata))){colnames(netdata) <- paste("var",1:ncol(netdata), sep="")}
    if(is.null(colnames(splitVars))){colnames(splitVars) <- paste("s.var",1:ncol(netdata), sep="")}
    d <- cbind(netdata, splitVars)
    f1 <- Formula::as.Formula(paste(c(paste(colnames(netdata),collapse=" + "), " ~ ", paste(colnames(splitVars), collapse=" + ")), collapse=""))
    n <- ncol(netdata)
    control<-NULL
    # Need to include n so cortrafo can count vars on left hand side
    tree <- partykit::ctree(formula=f1, data=d,
                            ytrafo=function(data, weights,control,...) {cortrafo(data=data, weights=weights, control=control, n=n,...)},
                            weights, na.action=na.action, ...)
    class(tree) <- c("networktree", "conditional", type[1], class(tree))
    res <- tree
  }
  return(res)
}

## formula interface

#' Tree model (partitioning) with networks at the end of branches
#'
#' Computes a tree model with networks at the end of branches. Can use
#' model-based recursive partitioning or conditional inference.
#'
#' Wraps the mob() and ctree() functions from the partykit package.
#'
#' @param formula A symbolic description of the model to be fit. This
#' should either be of type \code{y1 + y2 + y3 ~ x1 + x2} with node
#' vectors \code{y1}, \code{y2}, and \code{y3} or \code{y ~ x1 + x2}
#' with a matrix response {y}. \code{x1} and \code{x2} are used as
#' partitioning variables.
#' @param data a data frame containing the variables in the model
#' @param type the type of network to compute. Can be "cor", "pcor", or "EBICglasso"
#' @param method "ModelBased" or "Conditional"
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
#' tree1 <- networktree(formula = y1 + y2 + y3 ~ trend + foo, data=d)
#'
#'}
#'@export
networktree.formula <- function(formula, data, type=c("cor", "pcor", "EBICglasso"), 
                                method=c("ModelBased","Conditional"),
                                na.action, splitBy ="network", ...)
{
  if(method[1]=="ModelBased"){
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
    class(rval) <- c("networktree", "modelbased", type[1], class(rval))
    res <- rval
  } else if(method[1]=="Conditional"){
    charformulaLHS <-   strsplit(as.character(formula), "+", fixed=T)[[2]]
    n <- length(charformulaLHS)
    control<-NULL
    # Need to include n so cortrafo can count vars on left hand side
    res <- partykit::ctree(formula=formula, data=data,
                            ytrafo=function(data, weights,control,...) {cortrafo(data=data, weights=weights, control=control, n=n,...)},
                            weights,na.action=na.action, ...)
    class(res) <- c("networktree", "conditional", type[1], class(res))
  }
  return(res)
}


## methods for networktree class
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


# Package documentation
# TODO: fix this? causes a conflict w/networktree function documentation,
# I tried for over an hour and couldn't fix...

# networktree
#
#Recursive partitioning (tree models) of psychometric networks
#
#@details
#
#Includes methods for creating tree models with networks on the final branches.
#The methods use recursive partitioning on a multivariate normal distribution estimated
#from the data in order to separate distinct networks from one another.
#
#For a complete list of functions, use library(help = "networktree")
#
#For a complete list of vignettes, use browseVignettes("networktree")
#
# @author Payton J. Jones, Thorsten Simon, & Achim Zeleis
#"_PACKAGE"
