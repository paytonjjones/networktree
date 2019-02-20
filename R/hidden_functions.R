## ---- Hidden Functions ----

## net_terminal is a plotting function for the terminal nodes which is used internally in plot.networktree
## This uses grid package (with gridBase for qgraph)
net_terminal <- function (obj, type, which = NULL, id = TRUE, pop = TRUE, ylines = NULL,
                          mainlab = NULL, varlab = TRUE, bg = "white", ...) {

  y <- obj$fitted[["(response)"]]

  if (is.null(which))
    which <- 1L:NCOL(y)
  k <- length(which)
  rval <- function(node) {
    tid <- partykit::id_node(node)
    .nobs_party <- function(party, id=1L){
      dat <- partykit::data_party(party, id=id)
      if("(weights)" %in% names(dat)) {
        sum(dat[["(weights)"]])
      } else {nrow(dat)}
    }
    nobs <- .nobs_party(obj, id = tid)
    data <- partykit::data_party(obj, id=tid)
    top_vp <- grid::viewport(layout = grid::grid.layout(nrow = k, ncol = 2,
                               widths  = grid::unit(c(ylines, 1), c("lines", "null")),
                               heights = grid::unit(k, "null")),
                             width  = grid::unit(1, "npc"),
                             height = grid::unit(1, "npc") - grid::unit(2, "lines"),
                             name   = paste("node_mvar", tid, sep = ""))
    grid::pushViewport(top_vp)
    
    adj <- getnetwork(obj[[tid]], type=type)
    # Todo: delete lines below if working
    #net <- getnetwork(obj[[tid]], type=type)
    #g <- qgraph::qgraph(net, graph = "default", DoNotPlot = TRUE, ...)
    #adj <- qgraph::getWmat(g) 
   
    ## gridBase version
    ###########################
    
    ## plot white rectangle beneath qgraph
    grid::grid.rect(gp = grid::gpar(col = NA, fill = "white"))
    
    ## plot qgraph
    graphics::par(fig = gridBase::gridFIG(), mar = rep(0, 4), new = TRUE)
    qgraph::qgraph(adj, noPar = TRUE, ...)
    
    ###########################

    if (is.null(mainlab)) {
      mainlab <- if (id) {
        function(id, nobs) sprintf("Node %s (n = %s)",
                                   id, nobs)
      }
      else {
        function(id, nobs) sprintf("n = %s", nobs)
      }
    }
    if (is.function(mainlab)) {
      mainlab <- mainlab(tid, nobs)
    }
    for (i in 1L:k) {
      tmp <- obj
      tmp$fitted[["(response)"]] <- y[, which[i]]
      if (varlab) {
        nm <- names(y)[which[i]]
        if (i == 1L)
          nm <- paste(mainlab, nm, sep = ": ")
      }
      else {
        nm <- if (i == 1L)
          mainlab
        else ""
      }
      plot_vpi <- grid::viewport(layout.pos.col = 2L, layout.pos.row = i)
      grid::pushViewport(plot_vpi)
      if (pop)
        grid::popViewport()
      else grid::upViewport()
    }
    if (pop)
      grid::popViewport()
    else grid::upViewport()
  }
  return(rval)
}

## cortrafo is a general function for transforming a set of variables y1, y2, y3...
## into a matrix of (n^2-n)/2 columns (e.g., the number of total correlations)
## and i rows, where i is the # of observations of y1, where the mean of each vector
## is equal to the correlation between y1 and y2, y1 and y3, etc.
## used internally in ctree_net
# new cortrafo
cortrafo <- function(data, weights,control,n,model,...){
  data <- as.matrix(data$data[,data$variables$y,drop=FALSE])
  obs <- nrow(data)
  function(subset,weights,info,estfun,object,...){
    ef <- {
      scores <- NULL
      if(any("mean"        == model)) scores <- cbind(scores, data)
      if(any("variance"    == model)) scores <- cbind(scores, (data - mean(data))^2)
      if(any("correlation" == model)) {
        mymat <- matrix(list(), n,n)
        for(i in 1:n){
          for(j in 1:n){
            mymat[[i,j]] <- scale(data[,i]) * scale(data[,j])
          }
        }
        scores <- cbind(scores, matrix(unlist(mymat[lower.tri(mymat)]), obs, (n^2-n)/2))
      }
      scores
    }
    list(estfun=ef, unweighted=TRUE)
  }
}

useCortrafo <- function(data, weights,n,...){
  obs <- nrow(data)
  mymat <- matrix(list(), n,n)
  for(i in 1:n){
    for(j in 1:n){
      mymat[[i,j]] <- weights * scale(data[[i]]) * scale(data[[j]])
    }
  }
  matrix(unlist(mymat[lower.tri(mymat)]), obs, (n^2-n)/2)
}
