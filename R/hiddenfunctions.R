## ---- Hidden Functions ----

# ---- Terminal panel functions ---- 

ntqgraph <- function(obj,
                     transform,
                     layout = "lock",
                     pop = TRUE, 
                     bg = "white", 
                     minimum = 0,
                     maximum = "local",
                     ...) {
  
  coef_max_min <- get_coef_max_mins(obj)
  minimum <- ifelse(minimum[1] == "local", coef_max_min$min$edge, minimum)
  maximum <- ifelse(maximum[1] == "local", coef_max_min$min$edge, maximum)

  if(layout[1]=="lock"){
    layout <- qgraph::qgraph(getnetwork(obj,id=1),layout="spring",DoNotPlot=T)$layout
  } 
  
  rval <- function(node){
    # Set up parameters
    tid <- partykit::id_node(node)

    network <- getnetwork(obj, id = tid, transform = transform)

    # Set up viewport
    grid::pushViewport(grid::viewport())
    
    # PLOTTING
    
    ## plot white rectangle beneath 
    grid::grid.rect(gp = grid::gpar(col = NA, fill = bg))
    
    ## prep dimensions
    op <- graphics::par(no.readonly=TRUE)
    try(graphics::par(fig = detectPlotDimensions(), mar = rep(0, 4), new = TRUE), silent = TRUE)
    
    ## create base R plot
    qgraph::qgraph(network, noPar = TRUE, layout=layout, 
                   maximum = maximum, minimum = minimum,...)
    
    ## reset graphics to original settings
    graphics::par(op)
    
    if (pop)
      grid::popViewport()
    else grid::upViewport()
  }
}

ntbarplot <- function(obj,
                      sdbars = TRUE, 
                      bar_col_func=grDevices::colorRampPalette(c("yellow","darkred")),
                      pop = TRUE, 
                      bg = "white", 
                      na.rm = TRUE,
                      ...) {
  coef_max_min <- get_coef_max_mins(obj)

  rval <- function(node){
    # Set up parameters
    tid <- partykit::id_node(node)
    if("ctree_networktree" %in% class(obj)){
      response_data <- obj[tid]$fitted[['(response)']]
    } else if ("mob_networktree" %in% class(obj)) {
      response_names <- attr(obj[tid]$info$terms$response, "term.labels")
      response_data <- obj[tid]$data[,response_names]
    }
    means <- apply(response_data, 2, mean, na.rm=na.rm)
    sds <- apply(response_data, 2, sd, na.rm=na.rm)

    # Set up viewport
    grid::pushViewport(grid::viewport())
    
    # PLOTTING
    
    ## plot rectangle beneath 
    grid::grid.rect(gp = grid::gpar(col = NA, fill = bg))
    
    ## prep dimensions
    op <- graphics::par(no.readonly=TRUE)
    try(graphics::par(fig = detectPlotDimensions(), mar = rep(0, 4), new = TRUE), silent = TRUE)
    
    ## create base R plot
    graphics::par(mar = c(2.5, 2, 2, 2))
    
    bar_colors <- bar_col_func(100)
    bar_color_indices <- round(99 * (means - coef_max_min$min$mean) / 
                                 max(means - coef_max_min$min$mean), 0) + 1
    
    if(sdbars){
      # TODO: instead of using the maximum sd (biggest possible bar), I could actually
      # compute the lowest and highest bar length within get_coef_max_mins
      y_lim <- c(min(1.2 * (coef_max_min$min$mean - coef_max_min$max$sd/2), 0),
                 max(1.2 * (coef_max_min$max$mean + coef_max_min$max$sd/2), 0)) 
    } else {
      y_lim <- c(min(1.2 * coef_max_min$min$mean, 0),
                 max(1.2 * coef_max_min$max$mean, 0)) 
    }
    
    # Produce bar plot
    mid <- graphics::barplot(means, col=bar_colors[bar_color_indices],
                             ylim = y_lim, ...)
    
    if(sdbars){
      # Add st dev bars
      graphics::arrows(x0=mid, x1=mid,
                       y0=means - sds/2, 
                       y1=means + sds/2, 
                       code=3, angle=90, length=0.2*(1/length(sds)))
    }
    
    graphics::box()
    
    ## reset graphics to original settings
    graphics::par(op)
    
    if (pop)
      grid::popViewport()
    else grid::upViewport()
  }
}
class(ntbarplot) <- "grapcon_generator"

ntboxplot <- function(obj, 
                      col = "lightgray", 
                      pop = TRUE, 
                      bg = "white", 
                      na.rm = TRUE,
                      ...) {

  coef_max_min <- get_coef_max_mins(obj)
  ylim <- c(coef_max_min$min$mean - 3 * coef_max_min$max$sd,
            coef_max_min$max$mean + 3 * coef_max_min$max$sd)
    
  rval <- function(node){
    # Set up parameters
    tid <- partykit::id_node(node)
    data <- partykit::data_party(obj, id = tid)
    if("ctree_networktree" %in% class(obj)){
      response_data <- obj[tid]$fitted[['(response)']]
    } else if ("mob_networktree" %in% class(obj)) {
      response_names <- attr(obj[tid]$info$terms$response, "term.labels")
      response_data <- obj[tid]$data[,response_names]
    }
    means <- apply(response_data, 2, mean, na.rm=na.rm)
    sds <- apply(response_data, 2, sd, na.rm=na.rm)
	  k <- ncol(response_data)
	  obs <- nrow(response_data)
   
  	## put together bxp summary
  	bxp_sum <- list(
  		stats = rep.int(means, rep.int(5, k)) -
                  outer(c(3, 1, 0, -1, -3), unname(sds), `*`),
  		n = rep(obs, k),
  		conf = NULL, out = NULL, group = NULL,
  		names = colnames(response_data)
  	)
   
    # Set up viewport
    grid::pushViewport(grid::viewport())
    
    # PLOTTING
    
    ## plot rectangle beneath 
    grid::grid.rect(gp = grid::gpar(col = NA, fill = bg))
    
    ## prep dimensions
    op <- graphics::par(no.readonly=TRUE)
    try(graphics::par(fig = detectPlotDimensions(), mar = rep(0, 4), new = TRUE), silent = TRUE)
    
    ## create base R plot
    graphics::par(mar = c(2.5, 2, 2, 2))
    
    # Produce bar plot
    mid <- graphics::bxp(bxp_sum, boxfill = col, ylim = ylim,
		main = paste("id = ", tid, " / n =", obs), ...)
 
    graphics::box()
    
    ## reset graphics to original settings
    graphics::par(op)
    
    if (pop)
      grid::popViewport()
    else grid::upViewport()
  }
}
class(ntboxplot) <- "grapcon_generator"

ntmatplot <- function(obj, transform, col = grDevices::hcl.colors(11, "Blue-Red 3",
                      rev = TRUE), pop = TRUE, bg = "white", ...) {

  rval <- function(node){
    # Set up parameters
    tid <- partykit::id_node(node)
    network <- getnetwork(obj, id = tid, transform = transform)
    if("ctree_networktree" %in% class(obj)){
      response_data <- obj[tid]$fitted[['(response)']]
    } else if ("mob_networktree" %in% class(obj)) {
      response_names <- attr(obj[tid]$info$terms$response, "term.labels")
      response_data <- obj[tid]$data[,response_names]
    }
    k <- ncol(response_data)
    obs <- nrow(response_data)
    
    # Set up viewport
    grid::pushViewport(grid::viewport())
    
    # PLOTTING
    
    ## plot rectangle beneath 
    grid::grid.rect(gp = grid::gpar(col = NA, fill = bg))
    
    ## prep dimensions
    op <- graphics::par(no.readonly=TRUE)
    graphics::par(fig = detectPlotDimensions(), mar = rep(0, 4), new = TRUE)
    
    ## create base R plot
    graphics::par(mar = c(2.5, 2, 2, 2))
    
    # Produce plot
    graphics::image(1:k, 1:k, z = network[, rev(1:k)], zlim = c(-1, 1), col = col,
		main = paste("id = ", tid, " / n =", obs), axes = FALSE, asp = 1, ...)
    graphics::axis(1, at = 1:k, labels = as.character(1:k))
    graphics::axis(2, 1:k, rev(1:k), las = 1)
 
    graphics::box()
    
    ## reset graphics to original settings
    graphics::par(op)
    
    if (pop)
      grid::popViewport()
    else grid::upViewport()
  }
}
class(ntmatplot) <- "grapcon_generator"

# ---- Plotting helpers ---- 

get_coef_max_mins <- function(obj, na.rm = TRUE) {
    if("ctree_networktree" %in% class(obj)){
        response_data <- obj$fitted[['(response)']]
    } else if ("mob_networktree" %in% class(obj)) {
        response_names <- attr(obj$info$terms$response, "term.labels")
        response_data <- obj$data[,response_names]
    }

    get_measure_by_terminal_node <- function(response_var, FOO = mean) {
        tapply(response_var, obj$fitted['(fitted)'], FUN = FOO, na.rm = na.rm)
    }
    means <- apply(response_data, 2, get_measure_by_terminal_node, FOO = mean)
    sds   <- apply(response_data, 2, get_measure_by_terminal_node, FOO = sd)
  
    terminal_node_ids <- partykit::nodeids(obj, terminal = TRUE)
    k <- ncol(response_data)

    edges <- matrix(NA, nrow=length(terminal_node_ids), ncol=k*(k-1)*.5)
    for (i in seq_along(terminal_node_ids)) {
        # maxes & mins for edges default to -Inf/Inf if "correlation" is not in model
        # TODO: add error message for if they try to plot a network
        # without including "correlation" in model
        net <- try(getnetwork(obj, id=terminal_node_ids[i]), silent=TRUE)
        if (!inherits(net, "try-error")) {
            edges[i,] <- net[lower.tri(net)]
        }
    }
  
    return(
        list(
            min = list(
                mean = min(means, na.rm = na.rm),
                sd = min(sds, na.rm = na.rm),
                edge = suppressWarnings(min(edges, na.rm = na.rm))
            ),
            max = list(
                mean = max(means, na.rm = na.rm),
                sd = max(sds, na.rm = na.rm),
                edge = suppressWarnings(max(edges, na.rm = na.rm))
            )
        )
    )
}

detectPlotDimensions <- function() {
  
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

# ---- Other helpers ----

nettransform <- function(cor, n, labels, transform, verbose = FALSE){
  verbose_switch <- function(func){
    if(verbose){
      return(func)
    } else {
      return(suppressWarnings(suppressMessages(func)))
    }
  }
  net  <- verbose_switch(
    qgraph::getWmat(
      switch(transform[1],
             "cor"    = qgraph::qgraph(cor, graph = "default",
                                       DoNotPlot = TRUE, labels = labels),
             "pcor"   = qgraph::qgraph(cor, graph = "pcor",
                                       DoNotPlot = TRUE, labels = labels),
             "glasso" = qgraph::qgraph(Matrix::nearPD(cor)$mat,
                                       graph = "glasso", sampleSize = n,
                                       DoNotPlot = TRUE, labels = labels))))
  return(net)
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




