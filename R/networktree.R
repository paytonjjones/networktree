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
#' @references
#'
#' Jones, P.J., Mair, P., Simon, T., Zeileis, A. (2020). Network trees: A method for recursively partitioning covariance structures. Psychometrika, 85(4), 926-945. https://doi.org/10.1007/s11336-020-09731-4
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
#' ## plot 
#' plot(tree2)
#' plot(tree2, terminal_panel = "box")
#' plot(tree2, terminal_panel = "matrix")
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
#' @param method "mob" or "ctree"
#' @param model can be any combination of c("correlation", "mean", "variance")
#' splits are determined based on the specified characteristics
#' @param transform should stored correlation matrices be transformed to partial correlations
#' or a graphical lasso for plotting? Can be set to "cor" (default), "pcor", or "glasso"
#' @param na.action a function which indicates what should happen when the data
#' contain missing values (\code{NA}s).
#' @param weights weights
#' @param ... additional arguments passed to \code{\link[partykit]{mob_control}} (mob) 
#' or \code{\link[partykit]{ctree_control}} (ctree)
#'
#'@rdname networktree
#'@export
networktree.default <- function(nodevars, splitvars, 
                                method=c("mob","ctree"),
                                model ="correlation",
                                transform=c("cor", "pcor", "glasso"),
                                na.action=na.omit,
                                weights=NULL,...){
  nodevars <- formatnetworktreeinput(nodevars, prefix="nodevar")
  splitvars <- formatnetworktreeinput(splitvars, prefix="splitvar")

  if(method[1]=="mob"){
    d <- data.frame(nodevars,splitvars)
    form <- paste(paste(colnames(nodevars), collapse=" + "), "~",paste(colnames(splitvars), collapse=" + "))
    form <- as.formula(form)
    res <- networktree.formula(form, data = d, transform=transform, method=method, na.action=na.action, model = model, ...)
    
  } else if(method[1]=="ctree"){
    netdata <- as.data.frame(nodevars); splitvars <- as.data.frame(splitvars)
    d <- cbind(netdata, splitvars)
    f1 <- Formula::as.Formula(paste(c(paste(colnames(netdata),collapse=" + "), " ~ ", paste(colnames(splitvars), collapse=" + ")), collapse=""))
    tree <- partykit::ctree(formula=f1, data=d,
                            ytrafo=function(data, weights,control) {
                              cortrafo(data=data, weights=weights, control=NULL, model=model)
                              },
                             na.action=na.action, control=partykit::ctree_control(...))
    class(tree) <- c("networktree", "ctree_networktree", transform[1], class(tree))
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
networktree.formula <- function(formula, data, transform=c("cor", "pcor", "glasso"), 
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
		              Formula::model.part(F, stats::model.frame(F, data = data, rhs = 0), lhs = TRUE)
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
    class(rval) <- c("networktree", "mob_networktree", transform[1], class(rval))
    res <- rval
  } else if(method[1]=="ctree"){
    res <- partykit::ctree(formula=formula, data=data,
                            ytrafo=function(data, weights,control) {
                              cortrafo(data=data, weights=weights, control=NULL, model=model)
                              },
                            na.action=na.action, control=partykit::ctree_control(...))
    class(res) <- c("networktree", "ctree_networktree", transform[1], class(res))
    class(res$info$call) <- model ## discreetly store model
  }
  return(res)
}


## methods for networktree class

#' Printing 'networktree' objects
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

#' Plotting 'networktree' objects
#'
#' Wraps plot.party to plot a tree model with networks on the ends. Networks
#' are plotted with qgraph, and additional arguments are passed there
#'
#' @param x an object of type 'networktree'
#' @param terminal_panel an optional panel function of the form function(node)
#'        plotting the terminal nodes. Alternatively, a panel generating function
#'        of class "grapcon_generator" that is called with arguments x and tp_args
#'        to set up a panel function. Or, a character choosing one of the implemented
#'        standard plots \code{"graph"}, \code{"box"}, \code{"matrix"} or \code{"bar"}.
#'        The default (\code{NULL}) chooses an appropriate panel function depending
#'        on the "model" argument. 
#' @param transform "cor", "pcor", or "glasso". If set to NULL, transform detected from x
#' @param layout network layout, passed to qgraph. Default "lock" computes spring 
#' layout for the full sample and applies this to all graphs
#' @param sdbars if using a barplot, should std deviation error bars be plotted?
#' @param tnex terminal node extension (passed to plot.party). To make the terminal plots bigger, increase this value. 
#' @param partyargs additional arguments (list format) passed to \code{partykit::plot.party}
#' plotting function that takes partitioned data as input
#' @param na.rm should NA values be removed prior to calculating relevant parameters?
#' @param ... additional arguments passed to qgraph or barplot
#'
#'@export
plot.networktree <- function(x, 
                             terminal_panel = NULL, 
                             transform = NULL, 
                             layout = "lock", 
                             sdbars = FALSE,
                             tnex = 3,
                             partyargs=list(), 
                             na.rm=TRUE,
                             ...) {
  
  if("mob_networktree" %in% class(x)){
    model <- x[[1]]$info$dots$model
  } else {
    model <- class(x[[1]]$info$call)
  }
  
  if(is.null(transform)) {
    transform <- if("cor" %in% class(x)) {"cor"} else if ("pcor" %in% class(x)) {"pcor"
    } else if("glasso" %in% class(x)) {"glasso"
    } else {"glasso";
      warning("Type of network could not be detected, plotting glasso networks")}
  }
  
  if(is.function(terminal_panel)) {
    net_terminal_inner <- terminal_panel
  } else if (is.character(terminal_panel)) {
	terminal_panel <- match.arg(terminal_panel,
		c("graph", "barplot", "boxplot", "matrix"))
    net_terminal_inner <- switch(terminal_panel,
		"graph" = function(obj, ...) {
            ntqgraph(obj, transform = transform, layout = layout, ...)
        },
		"matrix" = function(obj, ...) {
		  ntmatplot(obj, transform = transform)
		},
		"barplot" = ntbarplot,
		"boxplot" = ntboxplot,
		stop("Undefined plotting type!")
	)
	class(net_terminal_inner) <- "grapcon_generator"
  }	else {	
    if("correlation" %in% model){
      net_terminal_inner <- function(obj, ...) {
        ntqgraph(obj, transform = transform, layout = layout, ...)
      }
    } else {
      if("variance" %in% model){
        net_terminal_inner <- ntboxplot
      } else {
        net_terminal_inner <- function(obj, ...) {
          ntbarplot(obj, sdbars = sdbars, ...)
        }
      }
    }
    class(net_terminal_inner) <- "grapcon_generator"
  }
  
  # Pass to partykit::plot.party
  dots <- list(...)
  if(!is.null(partyargs$tnex)){
    tnex <- partyargs$tnex
    message("The default tnex argument has been overwritten by input to partyargs")
  } else {
    partyargs$tnex <- tnex
  }
  needNewPlot <- tryCatch(
    {
      par(new=TRUE)
      FALSE
    }, 
    warning=function(cond){
      return(TRUE)
    },
    silent=TRUE
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


#' Predict 'networktree' objects
#'
#' Wraps predict.party
#'
#' @param object a fitted 'networktree'
#' @param newdata An optional data frame in which to look for variables with
#'        which to predict.  If omitted, the fitted values are used.
#' @param type "node", or "parameter". Specifies whether to predict nodes
#'        (return value is a vector) or parameters (matrix).
#' @param ... not used
#'
#' @method predict networktree
#'@export
predict.networktree <- function(object, newdata = NULL,
				type = c("node", "parameter"), ...) {
  type <- match.arg(type)

  ## predict node ids
  node <- partykit::predict.party(object, newdata = newdata)
  if(identical(type, "node")) {
    return(node)
  }

  ## obtain coefs
  stats::coef(object)[as.character(node), ]
}
