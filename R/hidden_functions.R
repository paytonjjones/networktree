## ---- Hidden Functions ----

## plotConditional is the generic plotting method for conditional plotting only
## layout gets passed to qgraph -- can be "circle" or "spring" (FR)
plotConditional <- function(x, type=NULL,layout=NULL, ...) {

  if(is.null(type)) {
    type <- if("cor" %in% class(x)) {"cor"} else if ("pcor" %in% class(x)) {"pcor"} else {"glasso"}
  }

  ## old ##
  #g <- ctree_qgraph(x[[1]])
  #adj <- qgraph::getWmat(g) ## note: recent untested change from coerce_to_adjacency
  #nms <- colnames(adj)
  #plot.new()
  #textHeights <- graphics::strheight(nms)*1.3
  #textWidths <- graphics::strwidth(nms)*1.3
  ## old (although plot.new might help) ##

  ## Create pngs for each child node
  imgList <- list()
  for(i in 1:length(x)){
    g <- ctree_qgraph(x[[i]])
    adj <- qgraph::getWmat(g) ## note: recent untested change from coerce_to_adjacency
    f <- file.path(tempdir(), "tempgraph")
    suppressMessages(qgraph(adj,filetype="png",filename=f,plot=T))
    imgList[[i]] <- png::readPNG(paste(f,".png",sep=""))
  }

  net_terminal_inner <- function(obj, ...) {
    net_terminal(obj, textHeights=textHeights, textWidths=textWidths,type=type,layout=layout,imgList=imgList, ...)
  }
  class(net_terminal_inner) <- "grapcon_generator"
  plot(x, terminal_panel=net_terminal_inner)
}


## net_terminal is a plotting function for the terminal nodes which is used internally in plot.networktree
## This uses grid package
net_terminal <- function (obj, textHeights, textWidths, type, layout=NULL, imgList, which = NULL, id = TRUE, pop = TRUE, ylines = NULL,
                          mainlab = NULL, varlab = TRUE, bg = "white", ...) {

  y <- obj$fitted[["(response)"]]
  if(is.null(layout)) {layout <- "circle"}

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
    top_vp <- viewport(layout = grid.layout(nrow = k, ncol = 2,
                                            widths = unit(c(ylines, 1), c("lines", "null")),
                                            heights = unit(k, "null")), width = unit(1, "npc"),
                       height = unit(1, "npc") - unit(2, "lines"), name = paste("node_mvar",
                                                                                tid, sep = ""))
    pushViewport(top_vp)

    g <- ctree_qgraph(obj[[tid]], type=type, layout=layout,...)
    adj <- qgraph::getWmat(g) ## Note: recent untested change from coerce_to_adjacency
    adj.sc <- 5*scale(adj, center=F)/max(scale(adj, center=F))
    vals <- adj[lower.tri(adj)]
    pts <- g$layout/4 + 0.5
    nms <- colnames(adj)
    fitted <- obj$fitted[["(fitted)"]]

    # ## plot the lines
    # for(i in 1:nrow(pts)){
    #   for(j in 1:nrow(pts)) {
    #     lcol <- ifelse(adj[i,j]>0, "forestgreen", "red")
    #     lwidth <- abs(adj.sc[i,j])
    #     grid.lines(x=c(pts[i,1], pts[j,1]),
    #                y=c(pts[i,2],pts[j,2]),
    #                gp=gpar(col=lcol,lwd=lwidth))
    #   }
    # }
    # ## plot text with white rectangle behind it
    # for(i in 1:nrow(pts)){
    #   grid.rect(x=pts[i,1], y=pts[i,2],
    #             width=textWidths[i], height=textHeights[i],
    #             gp=gpar(col="white"))
    #   grid.text(nms[i], x=pts[i,1], y=pts[i,2])
    # }

    ###########################
    ## Embed a base plot
    #par(plt=gridBase::gridFIG(), new=TRUE)
    #plot(1:10, axes=FALSE)
    #plot(qgraph(cor(depression), usePCH=FALSE, plot=F, filetype="", DoNotPlot=T))

    ## Embed a png with base
    #f <- file.path(tempdir(), "tempgraph")
    #qgraph(cor(depression), usePCH=FALSE, plot=T, filetype="png",filename=f)
    #mar <- gridBase::gridFIG()
    #par(plt=mar, new=TRUE)
    #img <- png::readPNG(paste(f,".png",sep=""))
    #rasterImage(image=img,
               # xleft=mar[1], xright=mar[2],
               # ybottom=mar[3], ytop=mar[4])

    ## Embed a png with grid
    grid.raster(image=imgList[[tid]])
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
      plot_vpi <- viewport(layout.pos.col = 2L, layout.pos.row = i)
      pushViewport(plot_vpi)
      if (pop)
        popViewport()
      else upViewport()
    }
    if (pop)
      popViewport()
    else upViewport()
  }
  return(rval)
}

## ctree_qgraph converts a terminal node into qgraph format
## used internally in plotting
ctree_qgraph <- function(terminal_node, method="qgraph", type=c("cor", "pcor", "GLASSO"),layout="circle",...){
  n <- ncol(terminal_node$fitted[['(response)']])
  sampleSize <- nrow(terminal_node$fitted[['(response)']])
  node_trans <- useCortrafo(data= terminal_node$fitted[['(response)']],
                         weights=terminal_node$fitted[['(weights)']],
                         n=n)
  cors <- apply(node_trans, 2, mean)
  cormat <- matrix(as.numeric(),n,n); diag(cormat) <- rep(1, n)
  cormat[upper.tri(cormat)]<- cors
  cormat[lower.tri(cormat)] <- t(cormat)[lower.tri(cormat)]
  colnames(cormat) <- rownames(cormat) <- names(terminal_node$fitted[['(response)']])
  net <- switch(type[1],
                "cor"=qgraph(cormat, graph="cor", layout=layout, DoNotPlot=T,...),
                "pcor"=qgraph(cormat, graph="pcor", layout=layout, DoNotPlot=T,...),
                "glasso"=qgraph(Matrix::nearPD(cormat)$mat, graph="glasso", sampleSize=sampleSize, layout=layout, DoNotPlot=T,...))
  return(net)
}

## cortrafo is a general function for transforming a set of variables y1, y2, y3...
## into a matrix of (n^2-n)/2 columns (e.g., the number of total correlations)
## and i rows, where i is the # of observations of y1, where the mean of each vector
## is equal to the correlation between y1 and y2, y1 and y3, etc.
## used internally in ctree_net
# new cortrafo
cortrafo <- function(data, weights,control,n,...){
  data <- data$data[,data$variables$y,drop=FALSE]
  obs <- nrow(data)
  vars <- n
  function(subset,weights,info,estfun,object,...){
    ef <- {
      mymat <- matrix(list(), n,n)
      for(i in 1:n){
        for(j in 1:n){
          mymat[[i,j]] <- scale(data[[i]]) * scale(data[[j]])
        }
      }
      matrix(unlist(mymat[lower.tri(mymat)]), obs, (n^2-n)/2)
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











################################################################################
################################################################################
################################################################################
################################################################################








## generates a png qgraph file
ctree_qgraph_png <- function(terminal_node, method="qgraph", type=c("cor", "pcor", "GLASSO"),layout="circle",...){
  n <- ncol(terminal_node$fitted[['(response)']])
  sampleSize <- nrow(terminal_node$fitted[['(response)']])
  node_trans <- useCortrafo(data= terminal_node$fitted[['(response)']],
                            weights=terminal_node$fitted[['(weights)']],
                            n=n)
  cors <- apply(node_trans, 2, mean)
  cormat <- matrix(as.numeric(),n,n); diag(cormat) <- rep(1, n)
  cormat[upper.tri(cormat)]<- cors
  cormat[lower.tri(cormat)] <- t(cormat)[lower.tri(cormat)]
  colnames(cormat) <- rownames(cormat) <- names(terminal_node$fitted[['(response)']])
  net <- switch(type[1],
                "cor"=qgraph(cormat, graph="cor", layout=layout,...),
                "pcor"=qgraph(cormat, graph="pcor", layout=layout,...),
                "glasso"=qgraph(Matrix::nearPD(cormat)$mat, graph="glasso", sampleSize=sampleSize, layout=layout,...))
  return(net)
}

## net_terminal is a new function I'm writing which will be used internally, but will use a png
## created by qgraph instead of plotting with grid
net_terminal_temp <- function(obj, ...){

  y <- obj$fitted[["(response)"]]
  if(is.null(layout)) {layout <- "circle"}

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

    g <- ctree_qgraph(obj[[tid]], type=type, layout=layout,...)
    adj <- qgraph::getWmat(g) ## Note: recent untested change from coerce_to_adjacency
    adj.sc <- 5*scale(adj, center=F)/max(scale(adj, center=F))
    vals <- adj[lower.tri(adj)]
    pts <- g$layout/4 + 0.5
    nms <- colnames(adj)
    fitted <- obj$fitted[["(fitted)"]]


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
    }
  }
  return(rval)
}

