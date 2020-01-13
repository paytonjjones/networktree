utils::globalVariables(c("na.pass"))

# S3 generic

#' @title networktree: Partitioning of network models
#' 
#' @description 
#' Computes a tree model with networks at the end of branches. Can use
#' model-based recursive partitioning or conditional inference.
#'
#' Wraps the mob() and ctree() functions from the partykit package.
#' 
#' Note: this package is in its early stages and the interface may change
#' for future versions.
#'
#' @references
#'
#' Jones PJ, Mair P, Simon T, Zeileis A (2019). “Network Model Trees.” OSF
#' ha4cw, OSF Preprints. doi: 10.31219/osf.io/ha4cw (URL:
#' https://doi.org/10.31219/osf.io/ha4cw).
#'
#' @examples
#' 
#' set.seed(1)
#' d <- data.frame(trend = 1:200, foo = runif(200, -1, 1))
#' d <- cbind(d, rbind(
#'   mvtnorm::rmvnorm(100, mean = c(0, 0, 0),
#'           sigma = matrix(c(1, 0.5, 0.5, 0.5, 1, 0.5, 0.5, 0.5, 1), ncol = 3)),
#'   mvtnorm::rmvnorm(100, mean = c(0, 0, 0),
#'           sigma = matrix(c(1, 0, 0.5, 0, 1, 0.5, 0.5, 0.5, 1), ncol = 3))
#' ))
#' colnames(d)[3:5] <- paste0("y", 1:3)
#'
#' ## Now use the function
#' tree1 <- networktree(nodevars=d[,3:5], splitvars=d[,1:2])
#' 
#' ## Formula interface
#' tree2 <- networktree(y1 + y2 + y3 ~ trend + foo, data=d)
#' 
#' \donttest{
#' ## Conditional version
#' tree3 <- networktree(nodevars=d[,3:5], splitvars=d[,1:2], 
#'                      method="ctree")
#' 
#' ## Change control arguments
#' tree4 <- networktree(nodevars=d[,3:5], splitvars=d[,1:2],
#'                      alpha=0.01)
#'}
#'@export
networktree <- function(...) UseMethod("networktree")

# Default method

#' @param nodevars the variables with which to compute the network. Can be vector, matrix, or dataframe
#' @param splitvars the variables with which to test split the network. Can be vector, matrix, or dataframe
#' @param type the type of network to compute. Can be "cor", "pcor", or "glasso". Note that networks
#' are always stored internally as correlation matrices, but will be auto-adjusted in plots etc. according
#' to type
#' @param method "mob" or "ctree"
#' @param model can be any combination of c("correlation", "mean", "variance")
#' splits are determined based on the specified characteristics
#' @param na.action a function which indicates what should happen when the data
#' contain missing values (\code{NA}s).
#' @param weights weights
#' @param ... additional arguments passed to \code{\link[partykit]{mob_control}} (mob) 
#' or \code{\link[partykit]{ctree_control}} (ctree)
#'
#'@rdname networktree
#'@export
networktree.default <- function(nodevars, splitvars, 
                                type=c("cor", "pcor", "glasso"),
                                method=c("mob","ctree"),
                                model ="correlation",
                                na.action=na.omit,
                                weights=NULL,...){
  nodevars <- as.matrix(nodevars)
  splitvars <- as.matrix(splitvars)
  if(is.null(colnames(nodevars))){colnames(nodevars) <- paste('nodevars',1:ncol(nodevars),sep="")}
  if(is.null(colnames(splitvars))){colnames(splitvars) <- paste('splitvars',1:ncol(splitvars),sep="")}
  
  if(method[1]=="mob"){
    d <- data.frame(nodevars,splitvars)
    form <- paste(paste(colnames(nodevars), collapse=" + "), "~",paste(colnames(splitvars), collapse=" + "))
    form <- as.formula(form)
    res <- networktree.formula(form, data = d, type=type, method=method, na.action=na.action, model = model, ...)
    
  } else if(method[1]=="ctree"){
    netdata <- as.data.frame(nodevars); splitvars <- as.data.frame(splitvars)
    d <- cbind(netdata, splitvars)
    f1 <- Formula::as.Formula(paste(c(paste(colnames(netdata),collapse=" + "), " ~ ", paste(colnames(splitvars), collapse=" + ")), collapse=""))
    n <- ncol(netdata)
    control<-NULL
    # Need to include n so cortrafo can count vars on left hand side
    tree <- partykit::ctree(formula=f1, data=d,
                            ytrafo=function(data, weights,control) {cortrafo(data=data, weights=weights, control=control, n=n, model=model)},
                             na.action=na.action, control=partykit::ctree_control(...))
    class(tree) <- c("networktree", "ctree_networktree", type[1], class(tree))
    class(tree$info$call) <- model ## discreetly store model
    res <- tree
  }
  return(res)
}

## formula interface

#' @param formula A symbolic description of the model to be fit. This
#' should either be of type \code{y1 + y2 + y3 ~ x1 + x2} with node
#' vectors \code{y1}, \code{y2}, and \code{y3} or \code{y ~ x1 + x2}
#' with a matrix response {y}. \code{x1} and \code{x2} are used as
#' partitioning variables.
#' @param data a data frame containing the variables in the model
#'@rdname networktree
#'@export
networktree.formula <- function(formula, data, type=c("cor", "pcor", "glasso"), 
                                method=c("mob","ctree"),
                                na.action=na.omit, model="correlation", ...)
{
  if(method[1]=="mob"){
    
    ## keep call
    cl <- match.call(expand.dots = TRUE)
    
    ## use dots for setting up mob_control
    control <- partykit::mob_control(...)
    control$ytype <- "matrix"

    ## set default for minsize if not specified
    if(is.null(control$minsize)) { 
	    F <- Formula::Formula(formula)
	    k <- ncol(stats::model.matrix(~ 0 + .,
		              model.part(F, stats::model.frame(F, data = data, rhs = 0), lhs = TRUE)
	              ))
	    control$minsize <- 2 * k + k * (k-1) / 2
    }
    
    ## control options for mvnfit
    mvncontrol <- list(model=model)
    
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
    class(rval) <- c("networktree", "mob_networktree", type[1], class(rval))
    res <- rval
  } else if(method[1]=="ctree"){
    charformulaLHS <-   strsplit(as.character(formula), "+", fixed=T)[[2]]
    n <- length(charformulaLHS)
    control <- NULL
    # Need to include n so cortrafo can count vars on left hand side
    res <- partykit::ctree(formula=formula, data=data,
                            ytrafo=function(data, weights,control) {cortrafo(data=data, weights=weights, control=control, n=n, model=model)},
                            na.action=na.action, control=partykit::ctree_control(...))
    class(res) <- c("networktree", "ctree_networktree", type[1], class(res))
    class(res$info$call) <- model ## discreetly store model
  }
  return(res)
}


## methods for networktree class

#' Printing 'treenetwork' objects
#'
#' Wraps print.modelparty to print a tree model with networks on the ends. 
#'
#' @param x an object of type 'networktree'
#' @param parameters print parameters for each partition? 
#' See getnetwork function for extracting parameters conveniently
#' @param FUN only evaluated if parameters=TRUE, passed to print.modelparty
#' @param ... additional arguments passed print.modelparty
#'
#'@export
print.networktree<- function(x,
                             parameters=FALSE,
                             FUN=NULL,
                             ...)
{
  FUN <- ifelse(!parameters, function(x){""}, FUN)
  partykit::print.modelparty(x, title = "Network tree object", FUN=FUN, ...)
}

#' Plotting 'treenetwork' objects
#'
#' Wraps plot.party to plot a tree model with networks on the ends. Networks
#' are plotted with qgraph, and additional arguments are passed there
#'
#' @param x an object of type 'networktree'
#' @param type "cor", "pcor", or "glasso". If set to NULL, type detected from x
#' @param layout network layout, passed to qgraph. Default "lock" computes spring 
#' layout for the full sample and applies this to all graphs
#' @param partyargs additional arguments (list format) passed to \code{partykit::plot.party}
#' @param ... additional arguments passed qgraph
#'
#'@export
plot.networktree <- function(x, type = NULL, layout="lock", partyargs=list(), ...) {
  
  dots <- list(...)
  
  if(is.null(type)) {
    type <- if("cor" %in% class(x)) {"cor"} else if ("pcor" %in% class(x)) {"pcor"
    } else if("glasso" %in% class(x)) {"glasso"
    } else {"glasso";
      warning("Type of network could not be detected, plotting glasso networks")}
  }
  
  if(layout[1]=="lock"){
    layout <- qgraph::qgraph(getnetwork(x,id=1),layout="spring",DoNotPlot=T)$layout
  }
  
  if("mob_networktree" %in% class(x)){
    model <- x[[1]]$info$dots$model
  } else {
    model <- class(x[[1]]$info$call)
  }
  if("variance" %in% model | "mean" %in% model){
    warning("Network plotting not yet implemented for splits by variance and mean.\nPrinting summary.")
    partyargs <- c(partyargs, list(x=x))
    do.call(what=partykit::plot.party,args=partyargs)
  } else {
    ## plotting network (when model == "correlation")
    net_terminal_inner <- function(obj, ...) {
      net_terminal(obj, type = type,layout = layout, ...)
    }
    class(net_terminal_inner) <- "grapcon_generator"
    needNewPlot <- tryCatch(
      {
        par(new=TRUE)
        FALSE
      }, 
      warning=function(cond){
        return(TRUE)
      },
      silent=T
    )
    if(needNewPlot){
      plot.new()
      partyargs <- c(partyargs, list(x=x, terminal_panel = net_terminal_inner, newpage=FALSE, tp_args = dots))
      do.call(what=partykit::plot.party,args=partyargs)
    } else {
      partyargs <- c(partyargs, list(x=x, terminal_panel = net_terminal_inner, newpage=TRUE, tp_args = dots))
      do.call(what=partykit::plot.party,args=partyargs)
    }
  }
}


# Package documentation
# TODO: fix package documentation. causes a conflict w/networktree function documentation
#       because they have the same name

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
