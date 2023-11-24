#' Creation of scenarii to weight the observation for modelisation with random forest
#'
#' @param Y_obs numeric vector
#' @param weight_for_non_prey numeric value
#'
#' @return a list of scenarii. Each scenario is a vector of weights
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # First load the data
#'
#' library(predipreda)
#' data(dataset)
#'
#' # The weight_scenarii_list function generate a weight list. Each object of the list is a scenario, which is a vector of length equal to the lenght of the response. Hence, each object of the vector is a weight.
#'
#' weight_list <- weight_scenarii_list(dataset$Response)
#' }

weight_scenarii_list <- function(Y_obs,weight_for_non_prey=NULL){
  if(is.null(weight_for_non_prey)){weight_for_non_prey <- 0.5}
  prey <- length(Y_obs[Y_obs>0])
  non_prey <- length(Y_obs[Y_obs==0])
  weight_vector_null <- rep(1,length(Y_obs))
  weight_vector_balanced <- ifelse(Y_obs>0, 1, prey/non_prey)
  weight_vector_weigthed <- ifelse(Y_obs>0, 1, weight_for_non_prey)
  weight_list <- list(weight_vector_null,weight_vector_balanced,weight_vector_weigthed)
  return(weight_list)
}

