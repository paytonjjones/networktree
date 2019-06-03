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

#' Ten Item Personality Questionnaire
#'
#' This dataset includes 1899 online participants who participated
#' in a questionnaire available through the Open Source Psychometrics
#' Project (https://openpsychometrics.org/), an organization that maintains an 
#' open website for the public to take psychometric tests for 
#' educational and entertainment purposes
#' 
#' The Ten Item Personality Questionnaire (TIPI) is a brief inventory of the Big 
#' Five personality domains. Each personality domain is assessed with two 
#' items. One item measures the domain normally and the other item measures 
#' the domain in reverse (e.g., "reserved, quiet" for reverse extraversion).
#' 
#' Labels for TIPI items in this dataset correspond to the first letter of 
#' each Big Five personality domain (Extraversion, Neuroticism, 
#' Conscientiousness, Agreeableness, and Openness to experience), 
#' with the character ``r'' indicating items that measure the domain 
#' in reverse.
#' 
#' Also includes demographics such as education level, 
#' rearing environment (urban/suburban/rural), gender, 
#' English as a native language, age, religion, sexual orientation, 
#' race, voting status, marriage status, and number of children 
#' in one's family during childhood. 
#' 
#' The dataset is publicly available at http://openpsychometrics.org/_rawdata/GCBS.zip
#' and can be cited as:
#' 
#' OpenPsychometrics (2019). Generic Conspiracist Beliefs Scale Survey. Retrieved from http://openpsychometrics.org/_rawdata/GCBS.zip
#'
#' @docType data
#'
#' @usage tipi
#'
#' @format a dataframe. Columns represent questionnaire items and rows represent individuals
#'
#' @keywords datasets
#'
#' @examples
#' head(tipi)
#' \donttest{
#' 
#' ## Example networktree with TIPI
#' data(tipi)
#' nodeVars <- c("E","A_r","C","N","O","E_r","A","C_r","N_r","O_r")
#' splitVars <- c("gender","education","engnat")
#' myTree<-networktree(tipi[,nodeVars], tipi[,splitVars])
#' myTree
#' plot(myTree)
#' 
#'}
#'
"tipi"

#' Depression Anxiety and Stress Scale
#'
#' This dataset includes a randomly selected subsample of 
#' 5000 online participants who participated
#' in a questionnaire available through the Open Source Psychometrics
#' Project (https://openpsychometrics.org/), an organization that maintains an 
#' open website for the public to take psychometric tests for 
#' educational and entertainment purposes
#' 
#' The Depression Anxiety and Stress Scale (DASS) is a self-report
#' instrument for measuring depression, anxiety, and tension or stress. 
#' Each of 42 items falls into one of the three corresponding subscales.
#' 
#' Labels for DASS items in this dataset are denoted by the prefix 
#' "dass" and the suffix "_D", "_A", or "_S", indicating the depression,
#' anxiety, or stress subscale. 
#' 
#' Also includes demographics such as country, education level, 
#' rearing environment (urban/suburban/rural), gender, 
#' English as a native language, age, religion, sexual orientation, 
#' race, voting status, marriage status, and number of children 
#' in one's family during childhood. 
#' 
#' The full dataset is publicly available at https://openpsychometrics.org/_rawdata/DASS_data_21.02.19.zip
#' and can be cited as:
#' 
#' OpenPsychometrics (2019). Depression Anxiety and Stress Scale Survey. Retrieved from https://openpsychometrics.org/_rawdata/DASS_data_21.02.19.zip
#'
#' @docType data
#'
#' @usage dass
#'
#' @format a dataframe. Columns represent questionnaire items and rows represent individuals
#'
#' @keywords datasets
#'
#' @examples
#' head(dass)
#' \donttest{
#' 
#' ## Example networktree with DASS
#' data(dass)
#' ## Select depression subscale
#' nodeVars <- colnames(dass)[(grep("_D", colnames(dass)))]
#' splitVars <- c("gender","orientation","race","married","engnat")
#' myTree<-networktree(dass[,nodeVars], dass[,splitVars])
#' myTree
#' plot(myTree)
#' 
#'}
#'
"dass"