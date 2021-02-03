#' @title comparetree
#' 
#' @description 
#' 
#' Quickly compares two partitions of a networktree object
#' 
#' @param tree a networktree object
#' @param id1 the first partition
#' @param id2 the second partition
#' @param transform should stored correlation matrices be transformed to partial correlations 
#' or graphical lasso? Can be set to "cor", "pcor", or "glasso". Defaults to automatic detection
#' @param highlights the number of comparisons to highlight
#' @param plot plot a comparison of the two partitions?
#' @param plot.type "compare" or "subtract". "compare" plots the two networks
#' side by side. "subtract" subtracts network 2 from network 1, and plots
#' a network where edge weights indicate the difference
#' @param layout layout for the plots. The default "constrained" uses a
#' FR layout from the full dataset 
#' @param ... additional arguments passed to qgraph
#'
#' @examples
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
#' ## Generate a networktree
#' tree1 <- networktree(nodevars=d[,3:5], splitvars=d[,1:2])
#' 
#' ## Print out the structure
#' tree1
#' 
#' ## Compare any two partitions
#' comparetree(tree1, id1=2, id2=3, highlights=3)
#' 
#' ## Add a comparison plot
#' comparetree(tree1, id1=2, id2=3, plot=TRUE)
#' 
#'@export
comparetree <- function(tree, id1=2L, id2=3L, 
                        transform = "detect", 
                        highlights=5,
                        plot=FALSE,
                        plot.type=c("compare","subtract"),
                        layout="constrained",
                        ...){
  
  part1 <- getnetwork(tree, id1, transform=transform)
  part2 <- getnetwork(tree, id2, transform=transform)
  comparison <- part1 - part2
  
  # create data table of top differences
  top <- reshape2::melt(part1, varnames= c("node1", "node2"), value.name="id1")
  top$id2 <- reshape2::melt(part2, varnames= c("node1", "node2"))$value
  top$'(id1 - id2)' <- reshape2::melt(comparison, varnames= c("node1", "node2"))$value
  
  # remove duplicates
  ord <- apply(top[,1:2], 1, sort)
  top[,1] <- ord[1,]; top[,2] <- ord[2,]
  top <- top[!duplicated(top),]
  # select top _ of highlights
  top <- top[order(abs(top$"(id1 - id2)"), decreasing=T),][1:min(highlights,nrow(top)),]
  rownames(top) <-NULL
  res <- list("highlights"=top, "matrix"=comparison)
  class(res) <- "comparetree"
  
  if(plot){
    if(is.character(layout)){
      if(layout=="constrained"){
      plot0 <- qgraph::qgraph(getnetwork(tree, id=1),DoNotPlot=T,layout="spring")
      layout <- plot0$layout
      }
    }
    # manage qgraph args
    dots <- list(...)
    if(is.null(dots$theme) & is.null(dots$posCol)){
      dots$posCol <- "#008585"
    }
    if(is.null(dots$theme) & is.null(dots$negCol)){
      dots$negCol <- "#C7522B"
    }
    if(match.arg(plot.type)=="compare"){
      op <- par(mfrow=c(1,2))
      do.call(what = qgraph::qgraph, args = c(dots, list(input=part1, layout=layout)))
      do.call(what = qgraph::qgraph, args = c(dots, list(input=part2, layout=layout)))
      par(op)
    } else if (match.arg(plot.type)=="subtract"){
      do.call(what = qgraph::qgraph, args = c(dots, list(input=part1-part2, 
                                                         layout=layout,
                                                         title=paste("Node ",id1, " - Node ", id2, sep=""))))
    }
  }
  return(res)
}

print.comparetree <- function(x, ...){
  print(x$highlights)
}

