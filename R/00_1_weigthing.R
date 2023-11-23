#' Creation of scenarii to weight the observation for modelisation with random forest
#'
#' @param Y_obs numeric vector
#' @param weight_for_non_prey numeric value
#'
#' @return
#' @export
#'
#' @examples
weight_scenarii_list <- function(Y_obs,weight_for_non_prey=NULL){
  if(!is.null(weight_for_non_prey)){weight_for_non_prey <- 0.5}
  prey <- length(Y_obs[Y_obs>0])
  non_prey <- length(Y_obs[Y_obs==0])
  weight_vector_null <- rep(1:length(Y_obs))
  weight_vector_balanced <- ifelse(Y_obs>0, 1, prey/non_prey)
  weight_vector_weigthed <- ifelse(Y_obs>0, 1, weight_for_non_prey)
  weight_list <- list(weight_vector_null,weight_vector_balanced,weight_vector_weigthed)
}

