#' Workaholism and Psychiatric Symptoms
#'
#' This dataset includes 16,426 workers who were assessed on symptoms
#' of psychiatric disorders (ADHD, OCD, anxiety, depression) and workaholism.
#' 
#' Scales: Adult ADHD Self-Report Scale, Obsession-Compulsive
#' Inventory-Revised, Hospital Anxiety and Depression Scale, and the Bergen
#' Work Addiction Scale.
#' 
#' Also includes demographics such as age, gender, work status, position, sector,
#' annual income. 
#' 
#' The dataset is publicly available at https://doi.org/10.1371/journal.pone.0152978
#' and can be cited as:
#' 
#' Andreassen, C. S., Griffiths, M. D., Sinha, R., Hetland, J., 
#' & Pallesen, S. (2016). The relationships between workaholism 
#' and symptoms of psychiatric disorders: a large-scale 
#' cross-sectional study. PloS One, 11, e0152978.
#'
#' @docType data
#'
#' @usage workaholic
#'
#' @format a dataframe. Columns represent symptoms and rows represent individuals
#'
#' @keywords datasets
#'
#' @examples
#' head(workaholic)
#' \donttest{
#' 
#' ## Example networktree with OCI-R scale
#' data(workaholic)
#' nodeVars <- paste("OCIR",1:18,sep="")
#' splitVars <- c("Workaholism_diagnosis","Gender")
#' myTree<-networktree(workaholic[,nodeVars], workaholic[,splitVars])
#' myTree
#' plot(myTree)
#' 
#'}
#'
"workaholic"

