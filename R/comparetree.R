#' @title comparetree
#' 
#' @description 
#' 
#' Quickly compares two partitions of a networktree object
#' 
#' @param tree a networktree object
#' @param id1 the first partition
#' @param id2 the second partition
#' @param type "cor", "pcor", or "glasso". Defaults to automatic detection
#' @param highlights the number of comparisons to highlight
#' @param plot plot a comparison of the two partitions?
#' @param layout layout for the plots. The default "constrained" uses a
#' FR layout from the full dataset 
#' @param ... additional arguments passed to qgraph
#'
#' @examples
#' 
#'@export
comparetree <- function(tree, id1=2L, id2=3L, 
                        type = "detect", 
                        highlights=5,
                        plot=FALSE,
                        layout="constrained",
                        ...){
  
  part1 <- getnetwork(tree, id1, type=type)
  part2 <- getnetwork(tree, id2, type=type)
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
    if(layout=="constrained"){
      plot0 <- qgraph::qgraph(getnetwork(tree, id=1),DoNotPlot=T,layout="spring")
      layout <- plot0$layout
    }
    op <- par(mfrow=c(1,2))
    plot1 <- qgraph::qgraph(part1, layout=layout,...)
    plot2 <- qgraph::qgraph(part2, layout=layout,...)
    par(op)
  }
  
  return(res)
}

print.comparetree <- function(x, ...){
  print(x$highlights)
}

