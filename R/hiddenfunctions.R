## ---- Hidden Functions ----

# ---- Scoring function ----

# TODO: this should be its own file and be exported
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
    
    ## fit coefs
    Sig <- cov(data) * (obs - 1)/obs
    
    list(estfun=ef, unweighted=TRUE,
         mvn = list(
           mu = colMeans(data)),
         sigma = sqrt(diag(Sig)),
         rho = cov2cor(Sig),
         ynam = if (is.null(colnames(data))) 1L:k else colnames(data)
    )
  }
}

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
                            labels= colnames(coef_list$rho), 
                            transform=transform)
    
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

# TODO: can I delete this?
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


