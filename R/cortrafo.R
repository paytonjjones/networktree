#' @title cortrafo
#' 
#' @description 
#' 
#' An influence function that transforms the response variables (y1, y2, y3...) into scores 
#' relevant to the correlations, means, and/or variances. 
#' 
#' For example, in the case of correlations the variables are transformed into a matrix of 
#' (n^2-n)/2 columns (e.g., the number of total correlations), and i rows, where i is the # of 
#' observations of y1, where the mean of each vector is equal to the correlation between y1 and y2, y1 and y3, etc.
#' 
#' Used internally in when method="ctree". 
#' 
#' @param data a matrix or data
#' @param weights not currently used
#' @param control not currently used
#' @param model can be any combination of c("correlation", "mean", "variance"). 
#' Scores are determined based on the specified characteristics
#' @param ... not currently used
#'
#'
#'@export
cortrafo <- function(data, weights, control, model, ...){
  data <- as.matrix(data$data[,data$variables$y,drop=FALSE])
  obs <- nrow(data)
  n <- ncol(data)
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
    
    list(estfun=ef, 
         unweighted=TRUE,
         mvn = list(
           mu = colMeans(data),
           sigma = sqrt(diag(Sig)),
           rho = cov2cor(Sig),
           ynam = if (is.null(colnames(data))) 1L:n else colnames(data)
           )
         )
  }
}