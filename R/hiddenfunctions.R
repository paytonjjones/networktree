## ---- Hidden Functions ----

# ---- Terminal panel functions ---- 

networkplot <- function(obj,
                        transform = NULL,
                        network = TRUE,
                        which = NULL, 
                        id = TRUE, 
                        pop = TRUE, 
                        mainlab = NULL, 
                        varlab = TRUE, 
                        bg = "white", 
                        ...) {
  # TODO:
  # coef_max_min <- get_coef_max_mins(obj)
  # once I have these, plug into 'maximum' and 'minimum' qgraph arguments
  
  rval <- function(node){
    # Set up parameters
    tid <- partykit::id_node(node)
    data <- partykit::data_party(obj, id=tid)
    coef_list <- partykit::info_node(node)$mvn
    
    network <- nettransform(cor = coef_list$rho, 
                            n = nrow(data),
                            labels = colnames(coef_list$rho), 
                            transform = transform)
    
    # Set up viewport
    grid::pushViewport(grid::viewport())
    
    # PLOTTING
    
    ## plot white rectangle beneath 
    grid::grid.rect(gp = grid::gpar(col = NA, fill = "white"))
    
    ## prep dimensions
    op <- graphics::par(no.readonly=TRUE)
    graphics::par(fig = detectPlotDimensions(), mar = rep(0, 4), new = TRUE)
    
    ## create base R plot
    qgraph::qgraph(network, noPar = TRUE, labels=colnames(network), ...)
    
    ## reset graphics to original settings
    graphics::par(op)
    
    if (pop)
      grid::popViewport()
    else grid::upViewport()
  }
}

# TODO: most params are not used, need to clean up here and in networkplot
ntbarplot <- function(obj,
                      sdbars = TRUE, 
                      bar_col_func=grDevices::colorRampPalette(c("yellow","darkred")),
                      transform = NULL,
                      network = TRUE,
                      which = NULL, 
                      id = TRUE, 
                      pop = TRUE, 
                      mainlab = NULL, 
                      varlab = TRUE, 
                      bg = "white", 
                      ...) {
  # TODO:
  # coef_max_min <- get_coef_max_mins(obj)
  # once I have these, substitute below
  
  rval <- function(node){
    # Set up parameters
    tid <- partykit::id_node(node)
    data <- partykit::data_party(obj, id=tid)
    coef_list <- partykit::info_node(node)$mvn
    
    # Set up viewport
    grid::pushViewport(grid::viewport())
    
    # PLOTTING
    
    ## plot white rectangle beneath 
    grid::grid.rect(gp = grid::gpar(col = NA, fill = "white"))
    
    ## prep dimensions
    op <- graphics::par(no.readonly=TRUE)
    graphics::par(fig = detectPlotDimensions(), mar = rep(0, 4), new = TRUE)
    
    ## create base R plot
    graphics::par(mar = c(2.5, 2, 2, 2))
    
    bar_colors <- bar_col_func(100)
    bar_color_indices <- round(99 * (coef_list$mu - min(coef_list$mu)) / max(coef_list$mu - min(coef_list$mu)), 0) + 1
    
    if(sdbars){
      y_lim <- c(min(coef_list$mu) - max(coef_list$sigma/2),
                 max(coef_list$mu) + max(coef_list$sigma/2)) * 1.2
    } else {
      y_lim <- NULL
    }
    
    # Produce bar plot
    mid <- graphics::barplot(coef_list$mu, col=bar_colors[bar_color_indices],
                             ylim = y_lim, ...)
    
    if(sdbars){
      # Add st dev bars
      graphics::arrows(x0=mid, x1=mid,
                       y0=coef_list$mu - coef_list$sigma/2, 
                       y1=coef_list$mu + coef_list$sigma/2, 
                       code=3, angle=90, length=0.2*(1/length(coef_list$sigma)))
    }
    
    graphics::box()
    
    ## reset graphics to original settings
    graphics::par(op)
    
    if (pop)
      grid::popViewport()
    else grid::upViewport()
  }
}

# ---- Plotting helpers ---- 

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




