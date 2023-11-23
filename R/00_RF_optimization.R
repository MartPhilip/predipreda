

#' Title
#'
#' @param response_data
#' @param trait_data
#' @param phylo_data
#' @param method
#' @param hyperparameter_list
#'
#' @return
#' @export
#'
#' @examples

#get the data
catm <- read.csv("data/raw-data/example_data.csv")
ev400 <- readRDS("data/raw-data/bird_eig400.rds")
IDn <- readRDS("data/raw-data/birdsID.rds")


#fix the rownames and get rid of "_"
IDn <- IDn[[2]]
ev400$ID <- rownames(ev400)
IDn$ID <- gsub(":",".", IDn$ID)
ev400 <- merge(ev400, IDn, by="ID", all.x=T)
cat <- catm[,!(names(catm)%in%c("X"))]
names(cat)[names(cat)=="binomial"] <- "targetTaxonName"

cat <- cat |>  dplyr::select(-volant,-Habitat,-ln.Beak.Depth,-ln.Beak.Length_Nares)
all <- cat
all$targetTaxonName <- gsub("_"," ", all$targetTaxonName)

#add phylo eigenvectors
ev400$ID <- NULL
names(ev400)[names(ev400)=="sp"] <- "binomial"
all <- merge(all, ev400, by.x ="targetTaxonName" ,by.y ="binomial", all.x=TRUE)
all <- all[complete.cases(all),] #all complete
all$response <- all$response/145

data <- all
response_data <- colnames(all)[2]
species <- colnames(all)[1]
trait_data <- all[colnames(cat)[-c(1:2)]]
phylo_data <- colnames(ev400[-401])
method <- "regression"

# hyper_grid <- expand.grid(
#   mtry_frac = c(.05, .15, .25), #floor(n_features *
#   min.node.size = c(1, 3, 5),
#   replace = c(TRUE),
#   sample.fraction = c(.5),
#   ntrees = seq(50),
#   PEMs = c(5,10)
# )

# hyper_grid <- expand.grid(
#   mtry_frac = c(.05), #floor(n_features *
#   min.node.size = c(1),
#   replace = c(TRUE),
#   sample.fraction = c(.5),
#   ntrees = seq(50),
#   PEMs = c(5)
# )

wgts <- all$response
wgts.0 <- rep(1,length(wgts))

hyperparameter_list <- NULL

opitmized_RF_function <- function(data,
                                  species,
                                  response_data,
                                  trait_data,
                                  phylo_data = NULL,
                                  method, # binary or regression
                                  weight,
                                  mtry_frac='NULL',
                                  min.node.size='NULL',
                                  sample.fraction='NULL',
                                  ntrees='NULL',
                                  wgt='NULL',
                                  PEMs='NULL')

  {
  if(is.null(phylo_data)){phylo<-FALSE}
  else{phylo<-TRUE}
  n_features <- dim(data)[2]-2

  formula <- as.formula(paste0(response_data, " ~ .-", species))

  rf1 <- ranger::ranger(formula = formula,
                        data = data,
                        mtry = floor(n_features / 3),
                        respect.unordered.factors = "order",
                        seed = 123,
                        case.weights = wgts.0)

  default_rmse <- sqrt(rf1$prediction.error)

  # execute full cartesian grid search, no weights assigned in this run
  start_time <- Sys.time()

  rmse <- vector()
  mtry <- vector()
  hyper_grid <- build_hyperparameter_dataframe(
      mtry_frac=mtry_frac,
      min.node.size=min.node.size,
      replace=replace,
      sample.fraction=sample.fraction,
      ntrees=ntrees,
      PEMs=PEMs,
      wgt=wgt,
      phylo=phylo)

  optimized_parameter <- pbmcapply::pbmclapply(1:nrow(hyper_grid), function(i) {
      if(phylo==TRUE){
      names1 <- colnames(data)[colnames(data) %in% colnames(trait_data)]
      names2 <- paste("eig", 1:hyper_grid$PEMs[i], sep = "")
      data_new <- data[c(species, response_data, names1, names2)]}
     if(phylo==FALSE){
      data_new <- data }

      fit <- ranger::ranger(formula = formula,
                            data = data_new,
                            num.trees = hyper_grid$ntrees[i],
                            mtry = round(hyper_grid$mtry_frac[i]*n_features_new),
                            min.node.size = hyper_grid$min.node.size[i],
                            replace = hyper_grid$replace[i],
                            sample.fraction = hyper_grid$sample.fraction[i],
                            verbose = FALSE,
                            seed = 123,
                            respect.unordered.factors = 'order')

      # export OOB error
      rmse[i] <- fit$prediction.error
      mtry[i] <- round(hyper_grid$mtry_frac[i]*n_features)

      final_objects <- data.frame(default_rmse = default_rmse,
                                  rmse = rmse[i],
                                  mtry = mtry[i])

      return(final_objects)

    # }

    }, mc.cores = parallel::detectCores() - 1)

  optimized_parameter_bind <- do.call(rbind, optimized_parameter)

  merged_optimized_parameter <- cbind(optimized_parameter_bind, hyper_grid)

  return(merged_optimized_parameter)

}

