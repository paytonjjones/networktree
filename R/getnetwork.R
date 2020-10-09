#' @title getnetwork
#' 
#' @description 
#' 
#' Easily extract a network from one of the 
#' nodes in a networktree object
#' 
#' @param tree a networktree object
#' @param id the node in the tree to extract. Use summary(tree) to see
#' id numbers for each split
#' @param transform should stored correlation matrices be transformed to partial correlations 
#' or graphical lasso? Can be set to "cor", "pcor", or "glasso". Defaults to automatic detection
#' @param verbose should warnings and messages from transformation functions (qgraph) be printed?
#' @param ... arguments passed to qgraph (e.g., "tuning", "threshold")
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
#' getnetwork(tree1, id=1)
#'
#'@export
getnetwork <- function(tree, id=1L, transform = "detect", verbose = FALSE,...){

  terminal_node <- tree[id]
  
  if(transform=="detect"){
    if("cor" %in% class(terminal_node)){
      transform <- "cor"
    } else if ("pcor" %in% class(terminal_node)){
      transform <- "pcor"
    } else if ("glasso" %in% class(terminal_node)){
      transform <- "glasso"
    } else {
      stop("Unable to detect transform")
    }
  }
  
  verbose_switch <- function(func){
    if(verbose){
      return(func)
    } else {
      return(suppressWarnings(suppressMessages(func)))
    }
  }
  
  if("ctree_networktree" %in% class(terminal_node)){
    n <- ncol(terminal_node$fitted[['(response)']])
    sampleSize <- nrow(terminal_node$fitted[['(response)']])
    node_trans <- useCortrafo(data= terminal_node$fitted[['(response)']],
                              weights=terminal_node$fitted[['(weights)']],
                              n=n)
    cors <- apply(node_trans, 2, mean, na.rm=T)
    matnames <- names(terminal_node$fitted[['(response)']])
  } else if ("mob_networktree" %in% class(terminal_node)){
    cors       <- terminal_node$node$info$coefficients
    cors       <- cors[grepl("rho", names(cors))] # select only cors, not mean/var
    matnames   <- attr(terminal_node$info$terms$response, "term.labels")
    n          <- length(matnames)
    sampleSize <- terminal_node$node$info$nobs
  }
  cormat <- matrix(as.numeric(),n,n); diag(cormat) <- rep(1, n)
  cormat[lower.tri(cormat)] <- cors
  cormat[upper.tri(cormat)] <- t(cormat)[upper.tri(cormat)]
  info <- list(cormat     = cormat,
              sampleSize = sampleSize)
  dots <- list(...)
  labels <- if(is.null(dots$labels)){matnames}else{dots$labels}
  
  net  <- verbose_switch(
            qgraph::getWmat(
              switch(transform[1],
                 "cor"    = qgraph::qgraph(info$cormat, graph = "default",
                                           DoNotPlot = TRUE, labels = labels, ...),
                 "pcor"   = qgraph::qgraph(info$cormat, graph = "pcor",
                                           DoNotPlot = TRUE, labels = labels, ...),
                 "glasso" = qgraph::qgraph(Matrix::nearPD(info$cormat)$mat,
                                           graph = "glasso", sampleSize = info$sampleSize,
                                           DoNotPlot = TRUE, labels = labels, ...))))
   return(net)
}


