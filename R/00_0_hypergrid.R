#' Creation of a hyperparameter dataset to use in ranger function to optimize a random forest model
#'
#' @param mtry_frac numeric vector of values
#' @param min.node.size numeric vector of values
#' @param sample.fraction numeric vector of values
#' @param ntrees numeric vector of values
#' @param wgt numeric vector of values
#'
#' @return
#' @export
#'
#' @examples
build_hyperparameter_data <- function(mtry_frac='NULL',
                                      min.node.size='NULL',
                                      sample.fraction='NULL',
                                      ntrees='NULL',
                                      wgt='NULL'){
  if(!is.null(mtry_frac)){mtry_frac <- c(.05, .15, .25, .333, .4, .6)}
  if(!is.null(min.node.size)){min.node.size <- c(1, 3, 5, 10, 20, 30, 50, 75, 100)}
  if(!is.null(sample.fraction)){sample.fraction = c(.5, .6, .7)}
  if(!is.null(ntrees)){ntrees <-seq(50,750,50)}
  if(!is.null(wgt)){wgt <- c(1)}
  replace <- c(TRUE, FALSE)
  mtry <- NA
  rmse <- NA
  hyper_grid <- base::expand.grid( mtry_frac = mtry_frac,
                                   min.node.size = min.node.size,
                                   replace = replace,
                                   sample.fraction = sample.fraction,
                                   ntrees = ntrees,
                                   mtry = mtry,
                                   rmse = rmse,
                                   wgt = wgt)
  return(hyper_grid)
}
