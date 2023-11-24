#' Building an hyperparameter dataset to optimized random forest using ranger from ranger
#'
#' @param mtry_frac a numeric vector
#' @param min.node.size a numeric vector
#' @param sample.fraction a numeric vector
#' @param ntrees a numeric vector
#' @param weight_list a list of numeric vector
#' @param PEMs a numeric vector
#' @param phylo a Boolean TRUE or FALSE
#'
#' @return
#' @export
#'
#' @examples
build_hyperparameter_dataframe <- function(mtry_frac=NULL,
                                      min.node.size=NULL,
                                      sample.fraction=NULL,
                                      ntrees=NULL,
                                      weight_list=weight_list,
                                      PEMs=NULL,
                                      phylo){
  if(phylo == FALSE){
  if(is.null(mtry_frac)){mtry_frac <- c(.05, .15, .25, .333, .4, .6)}
  if(is.null(min.node.size)){min.node.size <- c(1, 3, 5, 10, 20, 30, 50, 75, 100)}
  if(is.null(sample.fraction)){sample.fraction = c(.5, .6, .7)}
  if(is.null(ntrees)){ntrees <-seq(50,750,50)}
  wgt <- c(1:length(weight_list))
  replace <- c(TRUE, FALSE)
  hyper_grid <- base::expand.grid( mtry_frac = mtry_frac,
                                   min.node.size = min.node.size,
                                   replace = replace,
                                   sample.fraction = sample.fraction,
                                   ntrees = ntrees,
                                   wgt = wgt)
  return(hyper_grid)}
  if(phylo == TRUE){
    if(is.null(mtry_frac)){mtry_frac <- c(.05, .15, .25, .333, .4, .6)}
    if(is.null(min.node.size)){min.node.size <- c(1, 3, 5, 10, 20, 30, 50, 75, 100)}
    if(is.null(sample.fraction)){sample.fraction = c(.5, .6, .7)}
    if(is.null(ntrees)){ntrees <-seq(50,750,50)}
    wgt <- c(1:length(weight_list))
    if(is.null(PEMs)){PEMs <- c(1)}
    replace <- c(TRUE, FALSE)
    hyper_grid <- base::expand.grid( mtry_frac = mtry_frac,
                                     min.node.size = min.node.size,
                                     replace = replace,
                                     sample.fraction = sample.fraction,
                                     ntrees = ntrees,
                                     wgt = wgt,
                                     PEMs=PEMs)
    return(hyper_grid)}
}

