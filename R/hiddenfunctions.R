## ---- Hidden Functions ----

## terminalbase is a plotting function for the terminal nodes which is used internally in plot.networktree
## In conjunction with a baseplotfunction, it is used to plot a base R plot in the terminal nodes of a networktree
## It relies on the gridBase package
terminalbase <- function(obj,
                         baseplotfunction,
                         transform = NULL,
                         network = TRUE,
                         which = NULL, 
                         id = TRUE, 
                         pop = TRUE, 
                         ylines = NULL,
                         mainlab = NULL, 
                         varlab = TRUE, 
                         bg = "white", 
                         ...) {
  
  ## Initial Setup
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
    
    
    ## gridBase plotting 
    ###########################
    
    # Get the data / network that will be passed to baseplotfunction
    if(network){
      terminal_info <- getnetwork(obj[[tid]], transform=transform)
    } else {
      terminal_info <- getterminaldata(obj, tid)
    }
    
    dots <- list(...)

    # grid.Call() persistently fails in RStudio for Mac's default plot device
    # the detectPlotDimensions function catches this to provide a specific warning.
    #   It is adapted from gridBase::gridFIG()
    
    detectPlotDimensions <- function() {
      badFIG <- try(gridBase::gridFIG(), silent=TRUE)
      
      cvp <- currentViewportLoc() # the issue occurs here (traced to grid::grid.Call)
      din <- par("din")
      omi <- par("omi")
      
      if(inherits(badFIG, "try-error")) {
        warning("The grid package cannot accurately detect your plotting space. Use x11() or pdf() to plot.")
        partialWidth <- cvp$right - cvp$left
        cvp$right <- min(cvp$right, din[1])
        cvp$left <- cvp$right - partialWidth
      }
        
      width <- din[1] - omi[2] - omi[4]
      height <- din[2] - omi[1] - omi[3]
      
      fig <- round(c((cvp$left - omi[1])/width, (cvp$right - omi[1])/width, 
                     (cvp$bottom - omi[2])/height, (cvp$top - omi[2])/height), 
                   digits = 4)
      return(fig)
    }
    
    # this function is an internal from gridBase::: needed for detectPlotDimensions
    currentViewportLoc <- function() {
      transform <- grid::current.transform()
      width <- grid::convertWidth(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
      height <- grid::convertHeight(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
      bottomleft <- c(0, 0, 1) %*% transform
      left <- bottomleft[1]/bottomleft[3]
      bottom <- bottomleft[2]/bottomleft[3]
      topright <- c(width, height, 1) %*% transform
      right <- topright[1]/topright[3]
      top <- topright[2]/topright[3]
      list(left = left, bottom = bottom, right = right, top = top)
    }
    
    # PLOTTING
    
    ## plot white rectangle beneath 
    grid::grid.rect(gp = grid::gpar(col = NA, fill = "white"))
    
    ## prep dimensions
    op <- graphics::par(no.readonly=TRUE)
    graphics::par(fig = detectPlotDimensions(), mar = rep(0, 4), new = TRUE)

    ## create base R plot
    baseplotfunction(terminal_info, ...)
    
    ## reset graphics to original settings
    graphics::par(op)
    
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

baseplotfunction_network <- function(x, ...){
  qgraph::qgraph(x, noPar = TRUE, labels=colnames(x), ...)
}

baseplotfunction_bar <- function(x, 
                                 sdbars = TRUE, 
                                 na.rm=TRUE,
                                 bar_col_func=grDevices::colorRampPalette(c("yellow","darkred")),
                                 ...){
  
  graphics::par(mar = c(2.5, 2, 2, 2))
  
  stats <- list("mean"=colMeans(x, na.rm=na.rm),
                "variance"=apply(x, 2, stats::var, na.rm=na.rm))
  
  bar_colors <- bar_col_func(100)
  bar_color_indices <- round(99 * (stats$mean - min(stats$mean)) / max(stats$mean - min(stats$mean)), 0) + 1
  
  if(sdbars){
    y_lim <- c(min(stats$mean) - max(sqrt(stats$variance)/2),
               max(stats$mean) + max(sqrt(stats$variance)/2)) * 1.2
  } else {
    y_lim <- NULL
  }
  
  # Produce bar plot
  mid <- graphics::barplot(stats$mean, col=bar_colors[bar_color_indices],
                 ylim = y_lim, ...)
  
  if(sdbars){
    # Add st dev bars
    graphics::arrows(x0=mid, x1=mid,
                     y0=stats$mean - sqrt(stats$variance)/2, 
                     y1=stats$mean + sqrt(stats$variance)/2, 
                     code=3, angle=90, length=0.2*(1/length(stats$variance)))
  }
  
  graphics::box()
}


# to get data from the terminal nodes
getterminaldata <- function(tree, id=1L,...){
  terminal_node <- tree[id]
  if("ctree_networktree" %in% class(terminal_node)){
    response_data <- terminal_node$fitted[['(response)']]
  } else if ("mob_networktree" %in% class(terminal_node)){
    nodevar_names <- attr(terminal_node$info$terms$response, "term.labels")
    response_data <- terminal_node$data[,nodevar_names]
  }
  return(response_data)
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

formatnetworktreeinput <- function(vars, prefix="var"){
  if (!inherits(vars, "data.frame") && !inherits(vars, "matrix")) {
    vars <- data.frame(unlist(vars))
    colnames(vars) <- prefix
  }
  if (any(is.null(colnames(vars)))) {
    colnames(vars) <- paste(prefix,1:ncol(vars),sep="")
  }
  vars <- as.data.frame(vars)
  return(vars)
}
