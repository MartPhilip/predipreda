
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

weight_scenarii_list <- function(response_data,weight_for_non_prey=NULL){
  if(is.null(weight_for_non_prey)){weight_for_non_prey <- 0.5}
  prey <- length(response_data[response_data>0])
  non_prey <- length(response_data[response_data==0])
  weight_vector_null <- rep(1,length(response_data))
  weight_vector_balanced <- ifelse(response_data>0, 1, prey/non_prey)
  weight_vector_weigthed <- ifelse(response_data>0, 1, weight_for_non_prey)
  weight_list <- list(weight_vector_null,weight_vector_balanced,weight_vector_weigthed)
  return(weight_list)
}

